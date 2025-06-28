package parser

type AstVisitor interface {
	VisitProgram(program *Program)
	VisitFunction(function *Function)
	VisitParam(param *Param)
	VisitBlock(block *Block)
	VisitStatement(stmt Statement)
	VisitLiteral(literal *Literal)
	VisitVariableDeclaration(variableDeclaration *VariableDeclaration)
	VisitFunctionCall(functionCall *FunctionCall)
	VisitExpressionStatement(e *ExpressionStatement)
	VisitAssignment(assignment *Assignment)
	VisitVariableReference(variableReference *VariableReference)
	VisitReturnStatement(returnStmt *ReturnStatement)
	VisitBinaryOperation(binaryOp *BinaryOperation)
	VisitUnaryOperation(unaryOp *UnaryOperation)
	VisitIfStatement(ifSStmt *IfStatement)
	VisitWhileStatement(whileStmt *WhileStatement)
	VisitBreakStatement(breakStmt *BreakStatement)
	VisitContinueStatement(continueStmt *ContinueStatement)
}
