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
	VisitExpressionStatement(e *ExpressionStatement)
	VisitAssignment(assignment *Assignment)
	VisitVariableReference(variableReference *VariableReference)
	VisitReturnStatement(returnStatement *ReturnStatement)
	VisitBinaryOperation(binaryOperation *BinaryOperation)
	VisitUnaryOperation(unaryOperation *UnaryOperation)
	VisitIfStatement(ifStatement *IfStatement)
	VisitWhileStatement(whileStatement *WhileStatement)
}
