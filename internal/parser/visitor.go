package parser

type AstVisitor interface {
	VisitProgram(program *Program)
	VisitFunction(function *Function)
	VisitParam(param *Param)
	VisitBlock(block *Block)
	VisitStatement(statement Statement)
	VisitLiteral(literal *Literal)
	VisitVariableDeclaration(variableDeclaration *VariableDeclaration)
	VisitFunctionCall(functionCall *FunctionCall)
}
