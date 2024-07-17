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
  IsEqual
  IsNotEqual
  Less
  Greater
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

pub type Parameter =
  #(String, Type)

pub type Expression {
  Binary(left: Expression, op: Operator, right: Expression)
  Prefix(op: Operator, subject: Expression)
  Suffix(subject: Expression, op: Operator)
  Call(subject: Expression, args: List(Expression))
  AnonFunction(args: List(Parameter), body: Block)
  Literal(Literal)
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
  Function(name: String, args: List(Parameter), body: Block)
}

pub type Block =
  List(Declaration)
