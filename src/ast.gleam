import gleam/option
pub type Operator {
  Plus
  Minus
  Multiply
  Divide
  Bang
  Tilde
  Questionmark
  Dot
  And
  Or
}

pub type Literal {
    String(String)
    Number(Int)
    Boolean(Bool)
    Reference(String)
    Expression(Expression)
}

pub type Primitive {

}

pub type Type {
  Sum(List(Type))
  Product(List(Type))
  Just(Primitive)
}

pub type Argument = #(String, Type)

pub type Expression {
  Binary(left: Expression, op: Operator, right: Expression)
  Prefix(op: Operator, subject: Expression)
  Suffix(subject: Expression, op: Operator)
  Call(subject: Expression, args: List(Expression))
  Function(args: List(Argument), body: Block)
  Literal(Literal)
  MemberAccess(subject: Literal, member: String)
}

pub type Statement {
    Block(Block)
    ExpressionStmt(Expression)
    Return(option.Option(Expression))
    If(cond: Expression, body: Statement, else_block: option.Option(Statement))
    While(cond: Expression, body: Statement, else_block: option.Option(Statement))
    Assignment(variable: Declaration, value: Expression)
}

pub type Declaration {
    
}

pub type Block = List(Declaration)