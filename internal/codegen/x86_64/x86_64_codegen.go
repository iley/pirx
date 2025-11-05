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
	} else if assign, ok := op.(ir.AssignByAddr); ok {
		return generateAssignmentByAddr(cc, assign)
	} else if binop, ok := op.(ir.BinaryOp); ok {
		return generateBinaryOp(cc, binop)
	} else if unop, ok := op.(ir.UnaryOp); ok {
		return generateUnaryOp(cc, unop)
	} else if call, ok := op.(ir.Call); ok {
		return generateFunctionCall(cc, call), nil
	} else if call, ok := op.(ir.ExternalCall); ok {
		return generateExternalFunctionCall(cc, call)
	} else if ret, ok := op.(ir.Return); ok {
		return generateReturn(cc, ret), nil
	} else if ret, ok := op.(ir.ExternalReturn); ok {
		return generateExternalReturn(cc, ret)
	} else if jump, ok := op.(ir.Jump); ok {
		return []asm.Line{asm.Op1("jmp", asm.Ref(fmt.Sprintf(".L%s_%s", cc.functionName, jump.Goto)))}, nil
	} else if jumpUnless, ok := op.(ir.JumpUnless); ok {
		var lines []asm.Line
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, jumpUnless.Size), jumpUnless.Size, jumpUnless.Condition)...)
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(jumpUnless.Size), asm.Imm(0), asm.Reg(registerByIndex(0, jumpUnless.Size))))
		lines = append(lines, asm.Op1("je", asm.Ref(fmt.Sprintf(".L%s_%s", cc.functionName, jumpUnless.Goto))))
		return lines, nil
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
			// Load global's address into rcx (not rax, as rax is used by generateMemoryCopyToReg)
			lines = append(lines, generateAddressLoad(cc, "rcx", ir.Arg{Variable: assign.Target})...)
			lines = append(lines, generateMemoryCopyToReg(cc, assign.Value, assign.Size, "rcx", 0)...)
		} else {
			// Copy from source variable to target variable on stack
			lines = append(lines, generateMemoryCopyToReg(cc, assign.Value, assign.Size, "rbp", cc.locals[assign.Target])...)
		}
	} else if assign.Value.LiteralInt != nil {
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, assign.Size), assign.Size, assign.Value)...)
		lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, assign.Size), assign.Target)...)
	} else if assign.Value.LiteralFloat != nil {
		// Load float literal into xmm0 register
		lines = append(lines, generateFloatLiteralLoad(cc, "xmm0", assign.Size, *assign.Value.LiteralFloat)...)
		// Store from xmm0 to target variable
		lines = append(lines, generateStoreFloatToVariable(cc, "xmm0", assign.Size, assign.Target)...)
	} else if assign.Value.Zero {
		// Zero assignment
		lines = append(lines, generateRegisterLoad(cc, "rax", 8, assign.Value)...)
		offset := 0
		for offset < assign.Size {
			var reg string
			var stepSize int
			if assign.Size-offset >= 8 {
				reg = "rax"
				stepSize = 8
			} else if assign.Size-offset >= 4 {
				reg = "eax"
				stepSize = 4
			} else {
				reg = "al"
				stepSize = 1
			}

			if ir.IsGlobal(assign.Target) {
				if offset == 0 {
					// For first chunk with no offset, use direct store
					lines = append(lines, generateStoreToVariable(cc, reg, assign.Target)...)
				} else {
					// For chunks with offset, need to compute address
					label := getGlobalLabel(assign.Target)
					lines = append(lines, asm.Op2("leaq", asm.Arg{Label: label, Reg: "rip"}, asm.Reg("rcx")))
					if offset != 0 {
						lines = append(lines, asm.Op2("addq", asm.Imm(offset), asm.Reg("rcx")))
					}
					targetArg := asm.Arg{Reg: "rcx", Deref: true}
					switch stepSize {
					case 1:
						lines = append(lines, asm.Op2("movb", asm.Reg(reg), targetArg))
					case 4:
						lines = append(lines, asm.Op2("movl", asm.Reg(reg), targetArg))
					case 8:
						lines = append(lines, asm.Op2("movq", asm.Reg(reg), targetArg))
					}
				}
			} else {
				lines = append(lines, generateStoreToLocalWithOffset(cc, reg, assign.Target, offset)...)
			}
			offset += stepSize
		}
	} else {
		return lines, fmt.Errorf("unsupported assignment type: %v", assign)
	}

	return lines, nil
}

func generateAssignmentByAddr(cc *CodegenContext, assign ir.AssignByAddr) ([]asm.Line, error) {
	var lines []asm.Line

	// Handle multi-word assignments by copying in chunks
	if assign.Size > 8 {
		// Load the target address into rcx
		lines = append(lines, generateRegisterLoad(cc, "rcx", ast.WORD_SIZE, assign.Target)...)

		// Copy the value in chunks
		offset := 0
		for offset < assign.Size {
			var reg string
			var stepSize int

			if assign.Size-offset >= 8 {
				reg = "rax"
				stepSize = 8
			} else if assign.Size-offset >= 4 {
				reg = "eax"
				stepSize = 4
			} else {
				reg = "al"
				stepSize = 1
			}

			// Load chunk from source
			lines = append(lines, generateRegisterLoadWithOffset(cc, reg, stepSize, assign.Value, offset)...)

			// Store chunk to target address
			targetArg := asm.Arg{Reg: "rcx", Offset: offset, Deref: true}
			switch stepSize {
			case 1:
				lines = append(lines, asm.Op2("movb", asm.Reg(reg), targetArg))
			case 4:
				lines = append(lines, asm.Op2("movl", asm.Reg(reg), targetArg))
			case 8:
				lines = append(lines, asm.Op2("movq", asm.Reg(reg), targetArg))
			}

			offset += stepSize
		}
	} else {
		// For sizes <= 8, use the simple approach
		// Load the value into a register
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, assign.Size), assign.Size, assign.Value)...)

		// Load the target address into rcx
		lines = append(lines, generateRegisterLoad(cc, "rcx", ast.WORD_SIZE, assign.Target)...)

		// Store the value to the address in rcx
		targetArg := asm.Arg{Reg: "rcx", Deref: true}
		switch assign.Size {
		case 1:
			lines = append(lines, asm.Op2("movb", asm.Reg("al"), targetArg))
		case 4:
			lines = append(lines, asm.Op2("movl", asm.Reg("eax"), targetArg))
		case 8:
			lines = append(lines, asm.Op2("movq", asm.Reg("rax"), targetArg))
		default:
			return lines, fmt.Errorf("unsupported AssignByAddr size: %d", assign.Size)
		}
	}

	return lines, nil
}

func generateBinaryOp(cc *CodegenContext, binop ir.BinaryOp) ([]asm.Line, error) {
	var lines []asm.Line

	// Check if this is a floating point operation (operations ending with ".")
	isFloatOp := len(binop.Operation) >= 2 && binop.Operation[len(binop.Operation)-1:] == "."

	if isFloatOp {
		// For floating point operations, use XMM registers
		// Load left operand into xmm0
		lines = append(lines, generateFloatLoad(cc, "xmm0", binop.OperandSize, binop.Left)...)
		// Load right operand into xmm1
		lines = append(lines, generateFloatLoad(cc, "xmm1", binop.OperandSize, binop.Right)...)

		// Perform the operation
		switch binop.Operation {
		case "+.":
			lines = append(lines, asm.Op2("addsd", asm.Reg("xmm1"), asm.Reg("xmm0")))
		case "-.":
			lines = append(lines, asm.Op2("subsd", asm.Reg("xmm1"), asm.Reg("xmm0")))
		case "*.":
			lines = append(lines, asm.Op2("mulsd", asm.Reg("xmm1"), asm.Reg("xmm0")))
		case "/.":
			lines = append(lines, asm.Op2("divsd", asm.Reg("xmm1"), asm.Reg("xmm0")))
		default:
			return lines, fmt.Errorf("unsupported float binary operation: %v", binop.Operation)
		}

		// Store result from xmm0 to target variable
		lines = append(lines, generateStoreFloatToVariable(cc, "xmm0", binop.Size, binop.Result)...)

		return lines, nil
	}

	// Integer operations
	// Load operands into registers
	// Use rax and rcx (not rbx, since rbx holds the return value pointer in Pirx calling convention)
	lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, binop.OperandSize), binop.OperandSize, binop.Left)...)
	lines = append(lines, generateRegisterLoad(cc, registerByIndex(2, binop.OperandSize), binop.OperandSize, binop.Right)...)

	r0 := registerByIndex(0, binop.OperandSize)
	r1 := registerByIndex(2, binop.OperandSize) // Use register 2 (rcx) instead of 1 (rbx)

	switch binop.Operation {
	case "+":
		// addl %ebx, %eax (eax = eax + ebx)
		lines = append(lines, asm.Op2("add"+sizeToSuffix(binop.OperandSize), asm.Reg(r1), asm.Reg(r0)))
	case "-":
		// subl %ebx, %eax (eax = eax - ebx)
		lines = append(lines, asm.Op2("sub"+sizeToSuffix(binop.OperandSize), asm.Reg(r1), asm.Reg(r0)))
	case "*":
		// imull %ebx, %eax (eax = eax * ebx)
		lines = append(lines, asm.Op2("imul"+sizeToSuffix(binop.OperandSize), asm.Reg(r1), asm.Reg(r0)))
	case "/":
		// For division: need to sign-extend eax into edx:eax, then idivl
		switch binop.OperandSize {
		case 4:
			lines = append(lines, asm.Op0("cltd")) // sign-extend eax into edx:eax
		case 8:
			lines = append(lines, asm.Op0("cqo")) // sign-extend rax into rdx:rax
		}
		lines = append(lines, asm.Op1("idiv"+sizeToSuffix(binop.OperandSize), asm.Reg(r1)))
	case "%":
		// Same as division, but result is in edx
		switch binop.OperandSize {
		case 4:
			lines = append(lines, asm.Op0("cltd"))
		case 8:
			lines = append(lines, asm.Op0("cqo"))
		}
		lines = append(lines, asm.Op1("idiv"+sizeToSuffix(binop.OperandSize), asm.Reg(r1)))
		// Move remainder from edx to eax
		lines = append(lines, asm.Op2("mov"+sizeToSuffix(binop.OperandSize), asm.Reg(registerByIndex(3, binop.OperandSize)), asm.Reg(r0)))
	case "==":
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(binop.OperandSize), asm.Reg(r1), asm.Reg(r0)))
		lines = append(lines, asm.Op1("sete", asm.Reg("al")))
		lines = append(lines, asm.Op2("movzbl", asm.Reg("al"), asm.Reg("eax")))
	case "!=":
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(binop.OperandSize), asm.Reg(r1), asm.Reg(r0)))
		lines = append(lines, asm.Op1("setne", asm.Reg("al")))
		lines = append(lines, asm.Op2("movzbl", asm.Reg("al"), asm.Reg("eax")))
	case "<":
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(binop.OperandSize), asm.Reg(r1), asm.Reg(r0)))
		lines = append(lines, asm.Op1("setl", asm.Reg("al")))
		lines = append(lines, asm.Op2("movzbl", asm.Reg("al"), asm.Reg("eax")))
	case ">":
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(binop.OperandSize), asm.Reg(r1), asm.Reg(r0)))
		lines = append(lines, asm.Op1("setg", asm.Reg("al")))
		lines = append(lines, asm.Op2("movzbl", asm.Reg("al"), asm.Reg("eax")))
	case "<=":
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(binop.OperandSize), asm.Reg(r1), asm.Reg(r0)))
		lines = append(lines, asm.Op1("setle", asm.Reg("al")))
		lines = append(lines, asm.Op2("movzbl", asm.Reg("al"), asm.Reg("eax")))
	case ">=":
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(binop.OperandSize), asm.Reg(r1), asm.Reg(r0)))
		lines = append(lines, asm.Op1("setge", asm.Reg("al")))
		lines = append(lines, asm.Op2("movzbl", asm.Reg("al"), asm.Reg("eax")))
	case "&&":
		// Logical AND: result is 1 if both operands are non-zero
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(binop.OperandSize), asm.Imm(0), asm.Reg(r0)))
		lines = append(lines, asm.Op1("setne", asm.Reg("al")))
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(binop.OperandSize), asm.Imm(0), asm.Reg(r1)))
		lines = append(lines, asm.Op1("setne", asm.Reg("cl")))
		lines = append(lines, asm.Op2("andb", asm.Reg("cl"), asm.Reg("al")))
		lines = append(lines, asm.Op2("movzbl", asm.Reg("al"), asm.Reg("eax")))
	case "||":
		// Logical OR: result is 1 if either operand is non-zero
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(binop.OperandSize), asm.Imm(0), asm.Reg(r0)))
		lines = append(lines, asm.Op1("setne", asm.Reg("al")))
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(binop.OperandSize), asm.Imm(0), asm.Reg(r1)))
		lines = append(lines, asm.Op1("setne", asm.Reg("cl")))
		lines = append(lines, asm.Op2("orb", asm.Reg("cl"), asm.Reg("al")))
		lines = append(lines, asm.Op2("movzbl", asm.Reg("al"), asm.Reg("eax")))
	default:
		return lines, fmt.Errorf("unsupported binary operation: %v", binop.Operation)
	}

	// Store result to target variable (result is in register 0)
	lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, binop.Size), binop.Result)...)

	return lines, nil
}

func generateUnaryOp(cc *CodegenContext, op ir.UnaryOp) ([]asm.Line, error) {
	var lines []asm.Line

	switch op.Operation {
	case "!":
		// Logical NOT: result is 1 if operand is 0, 0 if operand is non-zero
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, op.Size), op.Size, op.Value)...)
		lines = append(lines, asm.Op2("cmp"+sizeToSuffix(op.Size), asm.Imm(0), asm.Reg(registerByIndex(0, op.Size))))
		lines = append(lines, asm.Op1("sete", asm.Reg("al")))
		lines = append(lines, asm.Op2("movzbl", asm.Reg("al"), asm.Reg("eax")))
		lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, op.Size), op.Result)...)
	case "-":
		// Negation
		lines = append(lines, generateRegisterLoad(cc, registerByIndex(0, op.Size), op.Size, op.Value)...)
		lines = append(lines, asm.Op1("neg"+sizeToSuffix(op.Size), asm.Reg(registerByIndex(0, op.Size))))
		lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, op.Size), op.Result)...)
	case "&":
		// Address-of
		lines = append(lines, generateAddressLoad(cc, registerByIndex(0, op.Size), op.Value)...)
		lines = append(lines, generateStoreToVariable(cc, registerByIndex(0, op.Size), op.Result)...)
	case "*":
		// Dereference
		lines = append(lines, generateMemoryCopyFromRef(cc, op.Value, op.Size, op.Result)...)
	default:
		return lines, fmt.Errorf("unsupported unary operation: %s", op.Operation)
	}

	return lines, nil
}

func sizeToSuffix(size int) string {
	switch size {
	case 1:
		return "b"
	case 4:
		return "l"
	case 8:
		return "q"
	default:
		panic(fmt.Errorf("unsupported size for suffix: %d", size))
	}
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
	// Additional arguments go on the stack
	// Return value in rax (or rax:rdx for values up to 16 bytes)

	var lines []asm.Line
	argRegisters := []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}
	nextRegister := 0
	var stackArgs []int // Indices of arguments that go on stack

	// First pass: load arguments into registers
	for i := range call.Args {
		argSize := call.ArgSizes[i]

		// Calculate how many registers we need
		needRegisters := 1
		if argSize > 8 {
			needRegisters = 2
		}

		// Check if we have enough registers
		if nextRegister+needRegisters > len(argRegisters) {
			// This argument and all remaining go on stack
			for j := i; j < len(call.Args); j++ {
				stackArgs = append(stackArgs, j)
			}
			break
		}

		// Load the argument into register(s)
		arg := call.Args[i]
		switch argSize {
		case 1:
			// Load 1-byte value and sign-extend to 64 bits
			reg8, reg32 := get8And32BitRegNames(argRegisters[nextRegister])
			lines = append(lines, generateRegisterLoad(cc, reg8, argSize, arg)...)
			lines = append(lines, asm.Op2("movsbl", asm.Reg(reg8), asm.Reg(reg32)))
		case 4:
			// Load 4-byte value and sign-extend to 64 bits
			reg32 := get32BitRegName(argRegisters[nextRegister])
			lines = append(lines, generateRegisterLoad(cc, reg32, argSize, arg)...)
			lines = append(lines, asm.Op2("movslq", asm.Reg(reg32), asm.Reg(argRegisters[nextRegister])))
		case 8:
			lines = append(lines, generateRegisterLoad(cc, argRegisters[nextRegister], argSize, arg)...)
		case 12:
			// Split across two registers: 8 bytes + 4 bytes
			lines = append(lines, generateRegisterLoadWithOffset(cc, argRegisters[nextRegister], 8, arg, 0)...)
			reg32 := get32BitRegName(argRegisters[nextRegister+1])
			lines = append(lines, generateRegisterLoadWithOffset(cc, reg32, 4, arg, 8)...)
		case 16:
			// Split across two 8-byte registers
			lines = append(lines, generateRegisterLoadWithOffset(cc, argRegisters[nextRegister], 8, arg, 0)...)
			lines = append(lines, generateRegisterLoadWithOffset(cc, argRegisters[nextRegister+1], 8, arg, 8)...)
		default:
			return lines, fmt.Errorf("unsupported external function argument size %d", argSize)
		}

		nextRegister += needRegisters
	}

	// Second pass: handle stack arguments
	var spShift int
	if len(stackArgs) > 0 {
		// Calculate space needed for stack arguments
		totalStackArgSize := 0
		for _, argIdx := range stackArgs {
			argSize := call.ArgSizes[argIdx]
			// Each argument takes at least 8 bytes on the stack
			if argSize <= 8 {
				totalStackArgSize += ast.WORD_SIZE
			} else {
				// Round up to multiple of 8
				totalStackArgSize += ((argSize + 7) / 8) * 8
			}
		}
		spShift = alignSP(totalStackArgSize)

		// Allocate space on stack first
		lines = append(lines, asm.Op2("subq", asm.Imm(spShift), asm.Reg("rsp")))

		// Store arguments onto stack (in correct order, not reversed)
		stackOffset := 0
		for _, argIdx := range stackArgs {
			arg := call.Args[argIdx]
			argSize := call.ArgSizes[argIdx]

			// For arguments that fit in a single 8-byte slot
			if argSize <= 8 {
				// Load argument into appropriate register and sign-extend to 64 bits
				switch argSize {
				case 1:
					lines = append(lines, generateRegisterLoad(cc, "al", argSize, arg)...)
					lines = append(lines, asm.Op2("movsbl", asm.Reg("al"), asm.Reg("eax")))
					lines = append(lines, asm.Op2("movslq", asm.Reg("eax"), asm.Reg("rax")))
				case 4:
					lines = append(lines, generateRegisterLoad(cc, "eax", argSize, arg)...)
					lines = append(lines, asm.Op2("movslq", asm.Reg("eax"), asm.Reg("rax")))
				case 8:
					lines = append(lines, generateRegisterLoad(cc, "rax", argSize, arg)...)
				default:
					return lines, fmt.Errorf("unsupported stack argument size %d", argSize)
				}

				// Store to stack at correct offset
				stackArg := asm.Arg{Reg: "rsp", Offset: stackOffset, Deref: true}
				lines = append(lines, asm.Op2("movq", asm.Reg("rax"), stackArg))
				stackOffset += ast.WORD_SIZE
			} else {
				// For larger arguments, copy 8-byte chunks
				bytesCopied := 0
				for bytesCopied < argSize {
					if argSize-bytesCopied >= 8 {
						lines = append(lines, generateRegisterLoadWithOffset(cc, "rax", 8, arg, bytesCopied)...)
						stackArg := asm.Arg{Reg: "rsp", Offset: stackOffset, Deref: true}
						lines = append(lines, asm.Op2("movq", asm.Reg("rax"), stackArg))
						bytesCopied += 8
						stackOffset += 8
					} else {
						// Handle remaining bytes
						lines = append(lines, generateRegisterLoadWithOffset(cc, "eax", 4, arg, bytesCopied)...)
						stackArg := asm.Arg{Reg: "rsp", Offset: stackOffset, Deref: true}
						lines = append(lines, asm.Op2("movl", asm.Reg("eax"), stackArg))
						bytesCopied += 4
						stackOffset += 4
					}
				}
			}
		}
	}

	label := call.Function
	if cc.features.FuncLabelsUnderscore {
		label = "_" + label
	}

	lines = append(lines, asm.Op1("call", asm.Ref(label)))

	// Clean up stack arguments if any
	if spShift > 0 {
		lines = append(lines, asm.Op2("addq", asm.Imm(spShift), asm.Reg("rsp")))
	}

	// Store result if needed
	if call.Result != "" {
		lines = append(lines, generateExternalResultStore(cc, call.Size, call.Result)...)
	}

	return lines, nil
}

func get32BitRegName(reg64 string) string {
	switch reg64 {
	case "rdi":
		return "edi"
	case "rsi":
		return "esi"
	case "rdx":
		return "edx"
	case "rcx":
		return "ecx"
	case "r8":
		return "r8d"
	case "r9":
		return "r9d"
	default:
		panic(fmt.Errorf("unknown 64-bit register: %s", reg64))
	}
}

func get8And32BitRegNames(reg64 string) (string, string) {
	switch reg64 {
	case "rdi":
		return "dil", "edi"
	case "rsi":
		return "sil", "esi"
	case "rdx":
		return "dl", "edx"
	case "rcx":
		return "cl", "ecx"
	case "r8":
		return "r8b", "r8d"
	case "r9":
		return "r9b", "r9d"
	default:
		panic(fmt.Errorf("unknown 64-bit register: %s", reg64))
	}
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
		// Load address of global into temp register, add offset, then load from that address
		// We'll use rcx as a temporary register (should be safe since we're in a load operation)
		labelArg := asm.Arg{Label: label, Reg: "rip"}
		lines = append(lines, asm.Op2("leaq", labelArg, asm.Reg("rcx")))

		// Add offset if non-zero
		if offset != 0 {
			lines = append(lines, asm.Op2("addq", asm.Imm(offset), asm.Reg("rcx")))
		}

		// Load from the computed address
		memArg := asm.Arg{Reg: "rcx", Deref: true}
		switch regSize {
		case 1:
			lines = append(lines, asm.Op2("movb", memArg, asm.Reg(reg)))
		case 4:
			lines = append(lines, asm.Op2("movl", memArg, asm.Reg(reg)))
		default:
			lines = append(lines, asm.Op2("movq", memArg, asm.Reg(reg)))
		}
	}

	return lines
}

func generateStoreToVariable(cc *CodegenContext, reg string, target string) []asm.Line {
	var lines []asm.Line

	if ir.IsGlobal(target) {
		label := getGlobalLabel(target)
		labelArg := asm.Arg{Label: label, Reg: "rip", Deref: true}

		regSize := registerSizeFromName(reg)
		switch regSize {
		case 1:
			lines = append(lines, asm.Op2("movb", asm.Reg(reg), labelArg))
		case 4:
			lines = append(lines, asm.Op2("movl", asm.Reg(reg), labelArg))
		default:
			lines = append(lines, asm.Op2("movq", asm.Reg(reg), labelArg))
		}
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

func generateFloatLiteralLoad(cc *CodegenContext, reg string, regSize int, literal float64) []asm.Line {
	label := cc.floatLiterals[literal]
	var lines []asm.Line

	// Load float literal from .rodata using RIP-relative addressing
	labelArg := asm.Arg{Label: label, Reg: "rip", Deref: true}

	switch regSize {
	case 8:
		// Load double (64-bit float) using movsd
		lines = append(lines, asm.Op2("movsd", labelArg, asm.Reg(reg)))
	case 4:
		// Load float (32-bit float) using movss
		lines = append(lines, asm.Op2("movss", labelArg, asm.Reg(reg)))
	default:
		panic(fmt.Errorf("unsupported float register size: %d", regSize))
	}

	return lines
}

func generateStoreFloatToVariable(cc *CodegenContext, reg string, regSize int, target string) []asm.Line {
	var lines []asm.Line

	if ir.IsGlobal(target) {
		// Store to global variable using RIP-relative addressing
		label := getGlobalLabel(target)
		labelArg := asm.Arg{Label: label, Reg: "rip", Deref: true}

		switch regSize {
		case 8:
			lines = append(lines, asm.Op2("movsd", asm.Reg(reg), labelArg))
		case 4:
			lines = append(lines, asm.Op2("movss", asm.Reg(reg), labelArg))
		default:
			panic(fmt.Errorf("unsupported float store size: %d", regSize))
		}
	} else {
		// Store to local variable on stack
		fullOffset := cc.locals[target]
		rbpArg := asm.Arg{Reg: "rbp", Offset: fullOffset, Deref: true}

		switch regSize {
		case 8:
			lines = append(lines, asm.Op2("movsd", asm.Reg(reg), rbpArg))
		case 4:
			lines = append(lines, asm.Op2("movss", asm.Reg(reg), rbpArg))
		default:
			panic(fmt.Errorf("unsupported float store size: %d", regSize))
		}
	}

	return lines
}

func generateFloatLoad(cc *CodegenContext, reg string, regSize int, arg ir.Arg) []asm.Line {
	var lines []asm.Line

	if arg.LiteralFloat != nil {
		// Load float literal
		lines = append(lines, generateFloatLiteralLoad(cc, reg, regSize, *arg.LiteralFloat)...)
	} else if arg.Variable != "" {
		// Load from variable
		if ir.IsGlobal(arg.Variable) {
			// Load from global variable using RIP-relative addressing
			label := getGlobalLabel(arg.Variable)
			labelArg := asm.Arg{Label: label, Reg: "rip", Deref: true}

			switch regSize {
			case 8:
				lines = append(lines, asm.Op2("movsd", labelArg, asm.Reg(reg)))
			case 4:
				lines = append(lines, asm.Op2("movss", labelArg, asm.Reg(reg)))
			default:
				panic(fmt.Errorf("unsupported float load size: %d", regSize))
			}
		} else {
			// Load from local variable on stack
			fullOffset := cc.locals[arg.Variable]
			rbpArg := asm.Arg{Reg: "rbp", Offset: fullOffset, Deref: true}

			switch regSize {
			case 8:
				lines = append(lines, asm.Op2("movsd", rbpArg, asm.Reg(reg)))
			case 4:
				lines = append(lines, asm.Op2("movss", rbpArg, asm.Reg(reg)))
			default:
				panic(fmt.Errorf("unsupported float load size: %d", regSize))
			}
		}
	} else {
		panic(fmt.Errorf("unsupported float load argument: %v", arg))
	}

	return lines
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

func generateMemoryCopyFromRef(cc *CodegenContext, sourceRef ir.Arg, size int, target string) []asm.Line {
	var lines []asm.Line

	// Load the source address into rcx
	lines = append(lines, generateRegisterLoad(cc, "rcx", ast.WORD_SIZE, sourceRef)...)

	// Copy from the address in rcx to target
	offset := 0
	for offset < size {
		if size-offset >= 8 {
			// Load 8 bytes from [rcx+offset] into rax
			srcArg := asm.Arg{Reg: "rcx", Offset: offset, Deref: true}
			lines = append(lines, asm.Op2("movq", srcArg, asm.Reg("rax")))
			lines = append(lines, generateStoreToLocalWithOffset(cc, "rax", target, offset)...)
			offset += 8
		} else if size-offset >= 4 {
			// Load 4 bytes from [rcx+offset] into eax
			srcArg := asm.Arg{Reg: "rcx", Offset: offset, Deref: true}
			lines = append(lines, asm.Op2("movl", srcArg, asm.Reg("eax")))
			lines = append(lines, generateStoreToLocalWithOffset(cc, "eax", target, offset)...)
			offset += 4
		} else {
			// Load 1 byte from [rcx+offset] into al
			srcArg := asm.Arg{Reg: "rcx", Offset: offset, Deref: true}
			lines = append(lines, asm.Op2("movb", srcArg, asm.Reg("al")))
			lines = append(lines, generateStoreToLocalWithOffset(cc, "al", target, offset)...)
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
