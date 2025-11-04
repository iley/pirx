package x86_64

import (
	"fmt"
	"maps"
	"slices"
	"strings"

	"github.com/iley/pirx/internal/asm"
	"github.com/iley/pirx/internal/ast"
	"github.com/iley/pirx/internal/codegen/common"
	"github.com/iley/pirx/internal/ir"
	"github.com/iley/pirx/internal/util"
)

const (
	FUNC_CALL_REGISTERS = 6 // rdi, rsi, rdx, rcx, r8, r9
)

type Features struct {
	VarargsOnStack       bool
	FuncLabelsUnderscore bool
}

type CodegenContext struct {
	features       Features
	stringLiterals map[string]string
	floatLiterals  map[float64]string

	// Function-specific.
	locals       map[string]int // name -> offset from rbp
	frameSize    int
	functionName string
}

func Generate(irp ir.IrProgram, features Features) (asm.Program, error) {
	asmProgram := asm.Program{}

	// Map from string to a label in the data section.
	stringLiterals := make(map[string]string)
	for i, s := range common.GatherStrings(irp) {
		stringLiterals[s] = fmt.Sprintf(".Lstr%d", i)
	}

	// Map from float to a label in the data section.
	floatLiterals := make(map[float64]string)
	for i, f := range common.GatherFloats(irp) {
		floatLiterals[f] = fmt.Sprintf(".Lflt%d", i)
	}

	globalVariables := common.GatherGlobals(irp)

	cc := &CodegenContext{
		features:       features,
		stringLiterals: stringLiterals,
		floatLiterals:  floatLiterals,
	}

	for _, f := range irp.Functions {
		fn, err := generateFunction(cc, f)
		if err != nil {
			return asmProgram, fmt.Errorf("error when generating code for function %s: %w", f.Name, err)
		}
		asmProgram.Functions = append(asmProgram.Functions, fn)
	}

	asmProgram.StringLiterals = generateStringLiterals(stringLiterals)
	asmProgram.FloatLiterals = generateFloatLiterals(floatLiterals)
	asmProgram.GlobalVariables = generateGlobalVariables(globalVariables)

	return asmProgram, nil
}

func generateStringLiterals(literals map[string]string) []asm.StringLiteral {
	var result []asm.StringLiteral
	for str, label := range literals {
		result = append(result, asm.StringLiteral{Label: label, Text: str})
	}
	return result
}

func generateFloatLiterals(literals map[float64]string) []asm.FloatLiteral {
	var result []asm.FloatLiteral
	for val, label := range literals {
		result = append(result, asm.FloatLiteral{Label: label, Value: val})
	}
	return result
}

func generateGlobalVariables(globals map[string]int) []asm.GlobalVariable {
	var result []asm.GlobalVariable

	sortedGlobals := slices.Collect(maps.Keys(globals))
	slices.Sort(sortedGlobals)

	for _, name := range sortedGlobals {
		label := getGlobalLabel(name)
		size := globals[name]
		result = append(result, asm.GlobalVariable{Label: label, Size: size})
	}

	return result
}

func generateFunction(cc *CodegenContext, irfn ir.IrFunction) (asm.Function, error) {
	result := asm.Function{
		Name: irfn.Name,
	}

	// Map from local variable name to its size in bytes.
	lsizes := make(map[string]int)

	// We need to know how many unique locals we have in total so we can allocate space on the stack below.
	for _, op := range irfn.Ops {
		if target := op.GetTarget(); target != "" {
			if ir.IsGlobal(target) {
				continue
			}
			if _, seen := lsizes[target]; seen {
				continue
			}
			lsizes[target] = op.GetSize()
		}
	}

	// Map from local variable name to its offset from rbp (negative).
	locals := make(map[string]int)

	// Sort the locals by size to waste less stack space on alignment.
	sortedLocals := slices.Collect(maps.Keys(lsizes))
	slices.SortFunc(sortedLocals, func(a, b string) int {
		if lsizes[a] != lsizes[b] {
			return lsizes[a] - lsizes[b]
		}
		return strings.Compare(a, b)
	})

	// Generate offsets from RBP for all locals.
	// Locals are stored at negative offsets from rbp.
	offset := 0
	for _, lname := range sortedLocals {
		align := min(8, lsizes[lname])
		offset = util.Align(offset, align)
		offset += lsizes[lname]
		locals[lname] = -offset // negative offset from rbp
	}

	// Space on the stack for local variables.
	// SP must always be 16-byte aligned.
	frameSize := alignSP(offset)
	result.Lines = append(result.Lines,
		asm.Comment(fmt.Sprintf("frame size: %d bytes", frameSize)))

	// Function arguments come in registers for C ABI or on stack above rbp for Pirx ABI.
	// For now, handle args on stack above rbp (similar to ARM64 approach).
	argsBlockSize := 0
	for _, argSize := range irfn.ArgSizes {
		argsBlockSize += argSize
	}
	// Add space for saved rbx (callee-saved register used for return address).
	argsBlockSize += ast.WORD_SIZE
	argsBlockSize = alignSP(argsBlockSize)

	// Function arguments are already on the stack above our frame (after the return address and saved rbp).
	// Calculate offsets for those.
	// Stack layout: ... [args] [return address] [saved rbp] [locals] ...
	//                                           ^rbp points here
	argOffset := argsBlockSize
	for i, arg := range irfn.Args {
		argOffset -= irfn.ArgSizes[i]
		locals[arg] = 16 + argOffset // 16 = saved rbp (8) + return addr (8) + saved rbx (8) - wait this doesn't add up
	}

	// Simplified: just use +16 for first arg
	argOffset = 16 // skip saved rbp (8) and return address (8)
	for i, arg := range irfn.Args {
		locals[arg] = argOffset
		argOffset += irfn.ArgSizes[i]
	}

	// Function prologue
	result.Lines = append(result.Lines,
		asm.Op1("pushq", asm.Reg("rbp")),
		asm.Op2("movq", asm.Reg("rsp"), asm.Reg("rbp")))

	// Allocate stack space for locals
	if frameSize > 0 {
		result.Lines = append(result.Lines,
			asm.Op2("subq", asm.Imm(frameSize), asm.Reg("rsp")))
	}

	cc.locals = locals
	cc.frameSize = frameSize
	cc.functionName = irfn.Name

	for i, op := range irfn.Ops {
		result.Lines = append(result.Lines,
			asm.Comment(fmt.Sprintf("Op %d: %s", i, op.String())))
		lines, err := generateOp(cc, op)
		if err != nil {
			return result, err
		}
		result.Lines = append(result.Lines, lines...)
	}

	// Function epilogue
	result.Lines = append(result.Lines,
		asm.Label(fmt.Sprintf(".L%s_exit", irfn.Name)),
		asm.Op2("movq", asm.Reg("rbp"), asm.Reg("rsp")),
		asm.Op1("popq", asm.Reg("rbp")),
		asm.Op0("ret"))

	return result, nil
}

func generateOp(cc *CodegenContext, op ir.Op) ([]asm.Line, error) {
	if assign, ok := op.(ir.Assign); ok {
		return generateAssignment(cc, assign)
	} else if call, ok := op.(ir.Call); ok {
		return generateFunctionCall(cc, call), nil
	} else if call, ok := op.(ir.ExternalCall); ok {
		return generateExternalFunctionCall(cc, call)
	} else if ret, ok := op.(ir.Return); ok {
		return generateReturn(cc, ret), nil
	} else if ret, ok := op.(ir.ExternalReturn); ok {
		return generateExternalReturn(cc, ret)
	} else if anchor, ok := op.(ir.Anchor); ok {
		return []asm.Line{asm.Label(fmt.Sprintf(".L%s_%s", cc.functionName, anchor.Label))}, nil
	} else {
		return nil, fmt.Errorf("unsupported op type: %T", op)
	}
}

func generateAssignment(cc *CodegenContext, assign ir.Assign) ([]asm.Line, error) {
	var lines []asm.Line

	if assign.Value.Variable != "" {
		// Variable to variable assignment
		if ir.IsGlobal(assign.Target) {
			// Load global's address into rax
			lines = append(lines, generateAddressLoad(cc, "rax", ir.Arg{Variable: assign.Target})...)
			lines = append(lines, generateMemoryCopyToReg(cc, assign.Value, assign.Size, "rax", 0)...)
		} else {
			// Copy from source variable to target variable on stack
			lines = append(lines, generateMemoryCopyToReg(cc, assign.Value, assign.Size, "rbp", cc.locals[assign.Target])...)
		}
	} else if assign.Value.LiteralInt != nil {
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, assign.Size), assign.Size, assign.Value)...)
		lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, assign.Size), assign.Target)...)
	} else if assign.Value.Zero {
		// Zero assignment
		lines = append(lines, generateRegisterLoad(cc, "rax", 8, assign.Value)...)
		offset := 0
		for offset < assign.Size {
			if assign.Size-offset >= 8 {
				lines = append(lines, generateStoreToLocalWithOffset(cc, "rax", assign.Target, offset)...)
				offset += 8
			} else if assign.Size-offset >= 4 {
				lines = append(lines, generateStoreToLocalWithOffset(cc, "eax", assign.Target, offset)...)
				offset += 4
			} else {
				lines = append(lines, generateStoreToLocalWithOffset(cc, "al", assign.Target, offset)...)
				offset += 1
			}
		}
	} else {
		return lines, fmt.Errorf("unsupported assignment type: %v", assign)
	}

	return lines, nil
}

func generateFunctionCall(cc *CodegenContext, call ir.Call) []asm.Line {
	// For Pirx-native functions we use a simpler calling convention:
	//  * All arguments go on the stack.
	//  * Caller always allocates space for the return value.
	//  * The return value's address is passed in via rbx (like x19 on ARM).

	var lines []asm.Line

	// Calculate total space needed for args and saved rbx
	argsSize := 0
	for _, argSize := range call.ArgSizes {
		argsSize += argSize
	}
	totalSize := argsSize + ast.WORD_SIZE // args + saved rbx
	totalSize = alignSP(totalSize)

	// Args need to end up at rbp+16 onwards in the callee.
	// Layout after subq $totalSize:
	//   [rsp+0 to rsp+(argsSize-1)]      = arguments
	//   [rsp+argsSize to rsp+(totalSize-1)] = saved rbx
	//
	// Before subq, we store:
	//   args at -totalSize onwards (growing upward in memory)
	//   rbx at -(totalSize - argsSize)

	// Store arguments starting from -totalSize
	argOffset := totalSize
	for i, arg := range call.Args {
		argSize := call.ArgSizes[i]
		lines = append(lines, generateMemoryCopyToReg(cc, arg, argSize, "rsp", -argOffset)...)
		argOffset -= argSize
	}

	// Save rbx after the arguments
	rbxSaveOffset := -(totalSize - argsSize)
	lines = append(lines, generateRegisterStore("rbx", "rsp", rbxSaveOffset)...)

	if call.Result != "" {
		// Store the result address in rbx.
		lines = append(lines, generateAddressLoad(cc, "rbx", ir.Arg{Variable: call.Result})...)
	}

	label := call.Function
	if cc.features.FuncLabelsUnderscore {
		label = "_" + label
	}

	// Adjust rsp, call, then restore rsp
	lines = append(lines,
		asm.Op2("subq", asm.Imm(totalSize), asm.Reg("rsp")),
		asm.Op1("call", asm.Ref(label)),
		asm.Op2("addq", asm.Imm(totalSize), asm.Reg("rsp")))

	// Restore rbx (using same offset as save)
	rbxArg := asm.Arg{Reg: "rsp", Offset: rbxSaveOffset, Deref: true}
	lines = append(lines, asm.Op2("movq", rbxArg, asm.Reg("rbx")))

	return lines
}

func generateExternalFunctionCall(cc *CodegenContext, call ir.ExternalCall) ([]asm.Line, error) {
	// Implement System V AMD64 ABI for external calls
	// First 6 integer/pointer arguments go in: rdi, rsi, rdx, rcx, r8, r9
	// Return value in rax

	var lines []asm.Line

	// Load arguments into registers
	// For now, only support up to 6 register arguments
	argRegisters := []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}

	for i, arg := range call.Args {
		if i >= len(argRegisters) {
			return lines, fmt.Errorf("more than 6 arguments not yet supported")
		}

		argSize := call.ArgSizes[i]
		reg := argRegisters[i]

		// Adjust register name for size
		if argSize == 4 {
			// Use 32-bit register variant
			switch reg {
			case "rdi":
				reg = "edi"
			case "rsi":
				reg = "esi"
			case "rdx":
				reg = "edx"
			case "rcx":
				reg = "ecx"
			case "r8":
				reg = "r8d"
			case "r9":
				reg = "r9d"
			}
		}

		lines = append(lines, generateRegisterLoad(cc, reg, argSize, arg)...)
	}

	label := call.Function
	if cc.features.FuncLabelsUnderscore {
		label = "_" + label
	}

	lines = append(lines, asm.Op1("call", asm.Ref(label)))

	// Store result if needed
	if call.Result != "" {
		lines = append(lines, generateExternalResultStore(cc, call.Size, call.Result)...)
	}

	return lines, nil
}

func generateExternalResultStore(cc *CodegenContext, size int, target string) []asm.Line {
	if size > 16 {
		panic("returning values larger than 16 bytes from external functions is not currently supported")
	}

	var lines []asm.Line
	offset := 0
	for offset < size {
		// On x86_64, return value can be split between RAX and RDX
		var retReg int
		if offset < 8 {
			retReg = 0 // rax
		} else {
			retReg = 3 // rdx
		}

		if size-offset >= 8 {
			lines = append(lines, generateStoreToLocalWithOffset(cc, registerByIndex(retReg, 8), target, offset)...)
			offset += 8
		} else if size-offset >= 4 {
			lines = append(lines, generateStoreToLocalWithOffset(cc, registerByIndex(retReg, 4), target, offset)...)
			offset += 4
			// Shift to get next 4 bytes if we're not at the end or register boundary
			if size > offset && offset != 8 {
				lines = append(lines, asm.Op2("shrq", asm.Imm(32), asm.Reg(registerByIndex(retReg, 8))))
			}
		} else {
			// Copy one byte at a time
			lines = append(lines, generateStoreToLocalWithOffset(cc, registerByIndex(retReg, 1), target, offset)...)
			offset += 1
			if size > offset {
				lines = append(lines, asm.Op2("shrq", asm.Imm(8), asm.Reg(registerByIndex(retReg, 8))))
			}
		}
	}

	return lines
}

func generateReturn(cc *CodegenContext, ret ir.Return) []asm.Line {
	var lines []asm.Line

	// Copy the return value into the slot provided by the caller via rbx.
	if ret.Value != nil {
		lines = append(lines, generateMemoryCopyToReg(cc, *ret.Value, ret.Size, "rbx", 0)...)
	}

	lines = append(lines, asm.Op1("jmp", asm.Ref(fmt.Sprintf(".L%s_exit", cc.functionName))))
	return lines
}

func generateExternalReturn(cc *CodegenContext, ret ir.ExternalReturn) ([]asm.Line, error) {
	var lines []asm.Line

	if ret.Value == nil {
		lines = append(lines, asm.Op1("jmp", asm.Ref(fmt.Sprintf(".L%s_exit", cc.functionName))))
		return lines, nil
	}

	// Load return value into rax
	switch ret.Size {
	case 4:
		lines = append(lines, generateRegisterLoad(cc, "eax", ret.Size, *ret.Value)...)
	case 8:
		lines = append(lines, generateRegisterLoad(cc, "rax", ret.Size, *ret.Value)...)
	default:
		return lines, fmt.Errorf("unsupported return value size: %d", ret.Size)
	}

	lines = append(lines, asm.Op1("jmp", asm.Ref(fmt.Sprintf(".L%s_exit", cc.functionName))))
	return lines, nil
}

func generateRegisterLoad(cc *CodegenContext, reg string, regSize int, arg ir.Arg) []asm.Line {
	return generateRegisterLoadWithOffset(cc, reg, regSize, arg, 0)
}

func generateRegisterLoadWithOffset(cc *CodegenContext, reg string, regSize int, arg ir.Arg, offset int) []asm.Line {
	if arg.Variable != "" {
		if ir.IsGlobal(arg.Variable) {
			return generateGlobalVariableLoadWithOffset(reg, regSize, arg.Variable, offset)
		} else {
			return generateLocalVariableLoadWithOffset(cc, reg, regSize, arg.Variable, offset)
		}
	} else if arg.LiteralInt != nil {
		if offset != 0 {
			panic("cannot load a literal int64 with offset")
		}
		return generateLiteralLoad(reg, regSize, *arg.LiteralInt)
	} else if arg.LiteralString != nil {
		if offset != 0 {
			panic("cannot load a literal string with offset")
		}
		if regSize != ast.WORD_SIZE {
			panic(fmt.Errorf("cannot load a string literal into register of size %d", regSize))
		}
		return generateStringLiteralLoad(cc, reg, *arg.LiteralString)
	} else if arg.Zero {
		return []asm.Line{asm.Op2("movq", asm.Imm(0), asm.Reg(reg))}
	} else {
		panic(fmt.Errorf("invalid arg in code generation: %v", arg))
	}
}

func generateLiteralLoad(reg string, regSize int, val int64) []asm.Line {
	var lines []asm.Line
	// Use appropriate mov instruction based on size
	switch regSize {
	case 1:
		lines = append(lines, asm.Op2("movb", asm.Imm(int(val)), asm.Reg(reg)))
	case 4:
		lines = append(lines, asm.Op2("movl", asm.Imm(int(val)), asm.Reg(reg)))
	default:
		lines = append(lines, asm.Op2("movq", asm.Imm(int(val)), asm.Reg(reg)))
	}
	return lines
}

func generateStringLiteralLoad(cc *CodegenContext, reg string, literal string) []asm.Line {
	label := cc.stringLiterals[literal]
	// Use LEA with RIP-relative addressing to load string address
	labelArg := asm.Arg{Label: label, Reg: "rip"}
	return []asm.Line{
		asm.Op2("leaq", labelArg, asm.Reg(reg)),
	}
}

func generateLocalVariableLoadWithOffset(cc *CodegenContext, reg string, regSize int, variable string, offset int) []asm.Line {
	var lines []asm.Line
	fullOffset := cc.locals[variable] + offset
	rbpArg := asm.Arg{Reg: "rbp", Offset: fullOffset, Deref: true}

	switch regSize {
	case 1:
		lines = append(lines, asm.Op2("movb", rbpArg, asm.Reg(reg)))
	case 4:
		lines = append(lines, asm.Op2("movl", rbpArg, asm.Reg(reg)))
	default:
		lines = append(lines, asm.Op2("movq", rbpArg, asm.Reg(reg)))
	}

	return lines
}

func generateGlobalVariableLoadWithOffset(reg string, regSize int, variable string, offset int) []asm.Line {
	var lines []asm.Line
	label := getGlobalLabel(variable)

	// Use RIP-relative addressing for position-independent code
	if offset == 0 {
		labelArg := asm.Arg{Label: label, Reg: "rip", Deref: true}
		switch regSize {
		case 1:
			lines = append(lines, asm.Op2("movb", labelArg, asm.Reg(reg)))
		case 4:
			lines = append(lines, asm.Op2("movl", labelArg, asm.Reg(reg)))
		default:
			lines = append(lines, asm.Op2("movq", labelArg, asm.Reg(reg)))
		}
	} else {
		panic("global variable load with offset not yet implemented")
	}

	return lines
}

func generateStoreToVariable(cc *CodegenContext, reg string, target string) []asm.Line {
	var lines []asm.Line

	if ir.IsGlobal(target) {
		panic("global variable store not yet implemented")
	} else {
		lines = append(lines, generateStoreToLocalWithOffset(cc, reg, target, 0)...)
	}

	return lines
}

func generateStoreToLocalWithOffset(cc *CodegenContext, reg string, target string, offset int) []asm.Line {
	var lines []asm.Line
	fullOffset := cc.locals[target] + offset
	rbpArg := asm.Arg{Reg: "rbp", Offset: fullOffset, Deref: true}

	regSize := registerSizeFromName(reg)
	switch regSize {
	case 1:
		lines = append(lines, asm.Op2("movb", asm.Reg(reg), rbpArg))
	case 4:
		lines = append(lines, asm.Op2("movl", asm.Reg(reg), rbpArg))
	default:
		lines = append(lines, asm.Op2("movq", asm.Reg(reg), rbpArg))
	}

	return lines
}

func registerByIndex(index, size int) string {
	// x86_64 register naming:
	// 64-bit: rax, rbx, rcx, rdx, rsi, rdi, r8-r15
	// 32-bit: eax, ebx, ecx, edx, esi, edi, r8d-r15d
	// 16-bit: ax, bx, cx, dx, si, di, r8w-r15w
	// 8-bit: al, bl, cl, dl, sil, dil, r8b-r15b

	var base string
	switch index {
	case 0:
		base = "a"
	case 1:
		base = "b"
	case 2:
		base = "c"
	case 3:
		base = "d"
	default:
		panic(fmt.Errorf("register index %d not yet implemented", index))
	}

	switch size {
	case 1:
		return base + "l"
	case 4:
		return "e" + base + "x"
	case 8:
		return "r" + base + "x"
	default:
		panic(fmt.Errorf("invalid register size %d", size))
	}
}

func registerSizeFromName(reg string) int {
	if len(reg) == 0 {
		panic("empty register name")
	}

	// 64-bit registers: rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp, r8-r15
	if reg[0] == 'r' {
		return 8
	}
	// 32-bit registers: eax, ebx, ecx, edx, esi, edi, esp, ebp
	if reg[0] == 'e' {
		return 4
	}
	// 8-bit registers: al, bl, cl, dl, etc.
	if len(reg) == 2 && reg[1] == 'l' {
		return 1
	}
	// 16-bit registers: ax, bx, cx, dx, etc.
	if len(reg) == 2 && reg[1] == 'x' {
		return 2
	}

	panic(fmt.Errorf("unknown register type: %s", reg))
}

func alignSP(offset int) int {
	// SP must always be aligned by 16 bytes on x86_64.
	return util.Align(offset, 16)
}

func getGlobalLabel(name string) string {
	return strings.TrimPrefix(name, "@")
}

func generateMemoryCopyToReg(cc *CodegenContext, source ir.Arg, size int, baseReg string, baseOffset int) []asm.Line {
	var lines []asm.Line
	offset := 0
	for offset < size {
		if size-offset >= 8 {
			lines = append(lines, generateRegisterLoadWithOffset(cc, "rax", 8, source, offset)...)
			lines = append(lines, generateRegisterStore("rax", baseReg, baseOffset+offset)...)
			offset += 8
		} else if size-offset >= 4 {
			lines = append(lines, generateRegisterLoadWithOffset(cc, "eax", 4, source, offset)...)
			lines = append(lines, generateRegisterStore("eax", baseReg, baseOffset+offset)...)
			offset += 4
		} else {
			lines = append(lines, generateRegisterLoadWithOffset(cc, "al", 1, source, offset)...)
			lines = append(lines, generateRegisterStore("al", baseReg, baseOffset+offset)...)
			offset += 1
		}
	}
	return lines
}

func generateRegisterStore(reg string, baseReg string, offset int) []asm.Line {
	var lines []asm.Line
	baseArg := asm.Arg{Reg: baseReg, Offset: offset, Deref: true}

	regSize := registerSizeFromName(reg)
	switch regSize {
	case 1:
		lines = append(lines, asm.Op2("movb", asm.Reg(reg), baseArg))
	case 4:
		lines = append(lines, asm.Op2("movl", asm.Reg(reg), baseArg))
	default:
		lines = append(lines, asm.Op2("movq", asm.Reg(reg), baseArg))
	}

	return lines
}

func generateAddressLoad(cc *CodegenContext, reg string, arg ir.Arg) []asm.Line {
	if arg.Variable == "" {
		panic(fmt.Errorf("can only load address of variables, got %s", arg))
	}

	var lines []asm.Line
	regArg := asm.Reg(reg)

	if ir.IsGlobal(arg.Variable) {
		label := getGlobalLabel(arg.Variable)
		// Use lea with RIP-relative addressing
		labelArg := asm.Arg{Label: label, Reg: "rip", Deref: false}
		lines = append(lines, asm.Op2("leaq", labelArg, regArg))
	} else {
		offset := cc.locals[arg.Variable]
		// lea reg, [rbp + offset]
		rbpArg := asm.Arg{Reg: "rbp", Offset: offset, Deref: false}
		lines = append(lines, asm.Op2("leaq", rbpArg, regArg))
	}

	return lines
}

func Optimize(p asm.Program) asm.Program {
	// No optimizations for now
	return p
}
