import gleam/list
import gleam/int

pub type Token {
  END
  Leftparen
  Rightparen
  Leftbracket
  Rightbracket
  Leftbrace
  Rightbrace
  Plus
  Minus
  Star
  Slash
  Percent
  And
  Or
  Less
  Greater
  Equals
  Bang
  Dot
  Questionmark
  Comma
  Colon
  Tilde
  Dequals
  Lessequals
  Greaterequals
  Notequals
  Leftshift
  Rightshift
  Pipe
  Bind
  Variable
  Value
  Function
  Return
  Tru
  Fals
  If
  Else
  While
  For
  In
  Match
  Area
  Structure
  Enumeration
  This
  Extend
  Extension
  Like
  String(String)
  Number(Int)
  Comment(String)
  Identifier(String)
  Whitespace(String)
}


pub fn repr_token(t: Token) -> String {
  case t {
    END             -> "End"
    Leftparen       -> "("
    Rightparen      -> ")"
    Leftbracket     -> "["
    Rightbracket    -> "]"
    Leftbrace       -> "{"
    Rightbrace      -> "}"
    Plus            -> "+"
    Minus           -> "-"
    Star            -> "*"
    Slash           -> "/"
    Percent         -> "%"
    And             -> "&"
    Or              -> "|"
    Less            -> "<"
    Greater         -> ">"
    Equals          -> "="
    Bang            -> "!"
    Dot             -> "."
    Questionmark    -> "?"
    Comma           -> ","
    Colon           -> ":"
    Tilde           -> "~"
    Dequals         -> "=="
    Lessequals      -> "<="
    Greaterequals   -> ">="
    Notequals       -> "!="
    Leftshift       -> "<<"
    Rightshift      -> ">>"
    Pipe            -> "|>"
    Bind            -> "->"
    Variable        -> "var"
    Value           -> "val"
    Function        -> "fun"
    Return          -> "return"
    Tru             -> "true"
    Fals            -> "false"
    If              -> "if"
    Else            -> "else"
    While           -> "while"
    For             -> "for"
    In              -> "in"
    Match           -> "match"
    Area            -> "area"
    Structure       -> "struct"
    Enumeration     -> "enum"
    This            -> "this"
    Extend          -> "extend"
    Extension       -> "extension"
    Like            -> "like"
    String(str)     -> "String: '"     <> str <> "'"
    Number(num)     -> "Number: "      <> num |> int.to_string
    Comment(str)    -> "Comment: #"    <> str
    Identifier(str) -> "Ident: "       <> str
    Whitespace(str) -> "Whitespace: '" <> str <> "'"
  }
}



pub fn repr_tokens(tokens: List(Token)) -> String {
  list.fold(from: "", over: tokens, with: fn(curr, next) {
      curr <> repr_token(next) <> ", "
    })
}

