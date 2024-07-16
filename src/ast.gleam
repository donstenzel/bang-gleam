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
  Less
  Greater
  IsEqual
  GreaterEquals
  LessEquals
  LeftShift
  RightShift
  Pipe
}

pub type Literal {
  String(String)
  Number(Int)
  Boolean(Bool)
  Reference(Reference)
  ParenExpression(Expression)
}

pub type Reference {
  Direct(String)
  Indirect(String, Reference)
}

pub type Primitive {
  Numeric
  Text
  Bool
}

pub type Type {
  Sum(Type, Type)
  Product(Type, Type)
  Just(Primitive)
}

pub type Argument =
  #(String, Type)

pub type Expression {
  Binary(left: Expression, op: Operator, right: Expression)
  Prefix(op: Operator, subject: Literal)
  Suffix(subject: Literal, op: Operator)
  Call(subject: Expression, args: List(Expression))
  AnonFunction(args: List(Argument), body: Block)
  Literal(Literal)
  MemberAccess(subject: Literal, member: String)
}

pub type Statement {
  Block(Block)
  Expression(Expression)
  Return(option.Option(Expression))
  If(cond: Expression, body: Statement, else_block: option.Option(Statement))
  While(cond: Expression, body: Statement, else_block: option.Option(Statement))
  Assignment(variable: Declaration, value: Expression)
}

pub type Declaration {
  Statement(Statement)
  Value(name: String, value: Expression)
  Variable(name: String, value: option.Option(Expression))
  Type(name: String, definition: Expression)
  Function(name: String, args: List(Argument), body: Block)
}

pub type Block =
  List(Declaration)
