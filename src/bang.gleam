// import gleam/erlang.{get_line as input}
import gleam/int
import gleam/io
import gleam/list
import gleam/string

pub fn main() {
  //input("...")

  let tokens = parse("fun a(b, c) { b + c }")

  io.print(
    list.fold(from: "", over: tokens, with: fn(curr, next) {
      curr <> repr(next) <> "\n"
    }),
  )



}

pub fn collapse(str: List(String)) -> String {
  case list.reduce(str, fn(s1, s2) { s2 <> s1 }) {
    Ok(str) -> str
    Error(_) -> ""
  }
}

pub fn eat(
  state: #(List(String), String),
  pred: fn(String) -> Bool,
) -> #(List(String), String) {
  // base cases: no input left, next char doesnt match
  let #(curr, str) = state

  case string.pop_grapheme(str) {
    Ok(#(grapheme, rest)) ->
      case pred(grapheme) {
        True -> eat(#([grapheme, ..curr], rest), pred)
        False -> #(curr, str)
      }
    Error(_) -> #(curr, str)
  }
}

pub fn parse(str: String) -> List(Token) {
  
  let eat_number = eat(_, fn(chr) { string.contains("01234679", chr) })
  
  let eat_whitespace = eat(_, fn(chr) { string.contains(" \t\n\r", chr) })
  
  let eat_ident = eat(_, fn(chr) { string.contains("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", chr) })
  
  let eat_comment_content = eat(_, fn(chr) { chr != "\n" })
  let eat_comment = fn(state: #(List(String), String)) {
    let #(_, rest) = state
    let assert Ok(#(_, rest)) = string.pop_grapheme(rest)
    let #(parsed, rest) = eat_comment_content(#([], rest))
    #("#" <> collapse(parsed), rest)
  }
  
  let eat_str_content = eat(_, fn(chr) { chr != "'" })
  let eat_string = fn(state: #(List(String), String)) {
    let #(_, rest) = state
    // we know that the first grapheme is ' since thats our condition to run into this.
    // not great since its coupled but i guess okay since its local
    let assert Ok(#(_, rest)) = string.pop_grapheme(rest)
    let #(parsed, rest) = eat_str_content(#([], rest))
    case string.pop_grapheme(rest) {
      Ok(#(grapheme, rest)) ->
        case grapheme {
          "'" -> #("'" <> collapse(parsed) <> "'", rest)
          _ -> panic
        }
      Error(_) -> panic
    }
  }

  case str {
    "" -> [END]
    "(" <> rest -> [Leftparen, ..parse(rest)]
    ")" <> rest -> [Rightparen, ..parse(rest)]
    "[" <> rest -> [Leftbracket, ..parse(rest)]
    "]" <> rest -> [Rightbracket, ..parse(rest)]
    "{" <> rest -> [Leftbrace, ..parse(rest)]
    "}" <> rest -> [Rightbrace, ..parse(rest)]
    "+" <> rest -> [Plus, ..parse(rest)]
    "-" <> rest -> [Minus, ..parse(rest)]
    "*" <> rest -> [Star, ..parse(rest)]
    "/" <> rest -> [Slash, ..parse(rest)]
    "%" <> rest -> [Percent, ..parse(rest)]
    "&" <> rest -> [And, ..parse(rest)]
    "|" <> rest -> [Or, ..parse(rest)]
    "<" <> rest -> [Less, ..parse(rest)]
    ">" <> rest -> [Greater, ..parse(rest)]
    "=" <> rest -> [Equals, ..parse(rest)]
    "!" <> rest -> [Bang, ..parse(rest)]
    "." <> rest -> [Dot, ..parse(rest)]
    "?" <> rest -> [Questionmark, ..parse(rest)]
    "," <> rest -> [Comma, ..parse(rest)]
    ":" <> rest -> [Colon, ..parse(rest)]
    "~" <> rest -> [Tilde, ..parse(rest)]
    "==" <> rest -> [Dequals, ..parse(rest)]
    "<=" <> rest -> [Lessequals, ..parse(rest)]
    ">=" <> rest -> [Greaterequals, ..parse(rest)]
    "!=" <> rest -> [Notequals, ..parse(rest)]
    "<<" <> rest -> [Leftshift, ..parse(rest)]
    ">>" <> rest -> [Rightshift, ..parse(rest)]
    "|>" <> rest -> [Pipe, ..parse(rest)]
    "->" <> rest -> [Bind, ..parse(rest)]
    "var" <> rest -> [Variable, ..parse(rest)]
    "val" <> rest -> [Value, ..parse(rest)]
    "fun" <> rest -> [Function, ..parse(rest)]
    "return" <> rest -> [Return, ..parse(rest)]
    "true" <> rest -> [Tru, ..parse(rest)]
    "false" <> rest -> [Fals, ..parse(rest)]
    "if" <> rest -> [If, ..parse(rest)]
    "else" <> rest -> [Else, ..parse(rest)]
    "while" <> rest -> [While, ..parse(rest)]
    "for" <> rest -> [For, ..parse(rest)]
    "in" <> rest -> [In, ..parse(rest)]
    "match" <> rest -> [Match, ..parse(rest)]
    "area" <> rest -> [Area, ..parse(rest)]
    "struct" <> rest -> [Structure, ..parse(rest)]
    "enum" <> rest -> [Enumeration, ..parse(rest)]
    "this" <> rest -> [This, ..parse(rest)]
    "extend" <> rest -> [Extend, ..parse(rest)]
    "extension" <> rest -> [Extension, ..parse(rest)]
    "like" <> rest -> [Like, ..parse(rest)]
    
    // non constant patterns:
    _ -> {
      let assert Ok(#(grapheme, _)) = string.pop_grapheme(str) 
      case grapheme {
        "#" -> {
          let #(parsed, rest) = eat_comment(#([], str))
          [Comment(parsed), ..parse(rest)]
        }

        " " | "\t" | "\n" -> {
          let #(parsed, rest) = eat_whitespace(#([], str))
          [Whitespace(collapse(parsed)), ..parse(rest)]
        }

        "'" -> {
          let #(parsed, rest) = eat_string(#([], str))
          [String(parsed), ..parse(rest)]
        }

        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> {
          let #(parsed, rest) = eat_number(#([], str))
          let assert Ok(num) = int.parse(collapse(parsed))
          [Number(num), ..parse(rest)]
        }

        "a" | "A" | "b" | "B" | "c" | "C" | "d" | "D" | "e" | "E"
      | "f" | "F" | "g" | "G" | "h" | "H" | "i" | "I" | "j" | "J"
      | "k" | "K" | "l" | "L" | "m" | "M" | "n" | "N" | "o" | "O"
      | "p" | "P" | "q" | "Q" | "r" | "R" | "s" | "S" | "t" | "T"
      | "u" | "U" | "v" | "U" | "w" | "W" | "x" | "X" | "y" | "Y"
      | "z" | "Z" | "_" -> {
          let #(parsed, rest) = eat_ident(#([], str))
          [Identifier(collapse(parsed)), ..parse(rest)]
        }

        _ -> panic as "Unknown character."
      }
    }
  }
}

pub fn repr(t: Token) -> String {
  case t {
    END -> "End of file"
    Leftparen -> "("
    Rightparen -> ")"
    Leftbracket -> "["
    Rightbracket -> "]"
    Leftbrace -> "{"
    Rightbrace -> "}"
    Plus -> "+"
    Minus -> "-"
    Star -> "*"
    Slash -> "/"
    Percent -> "%"
    And -> "&"
    Or -> "|"
    Less -> "<"
    Greater -> ">"
    Equals -> "="
    Bang -> "!"
    Dot -> "."
    Questionmark -> "?"
    Comma -> ","
    Colon -> ":"
    Tilde -> "~"
    Dequals -> "=="
    Lessequals -> "<="
    Greaterequals -> ">="
    Notequals -> "!="
    Leftshift -> "<<"
    Rightshift -> ">>"
    Pipe -> "|>"
    Bind -> "->"
    Variable -> "var"
    Value -> "val"
    Function -> "fun"
    Return -> "return"
    Tru -> "true"
    Fals -> "false"
    If -> "if"
    Else -> "else"
    While -> "while"
    For -> "for"
    In -> "in"
    Match -> "match"
    Area -> "area"
    Structure -> "struct"
    Enumeration -> "enum"
    This -> "this"
    Extend -> "extend"
    Extension -> "extension"
    Like -> "like"
    String(str) -> "String: " <> str
    Number(num) -> "Number: " <> int.to_string(num)
    Comment(str) -> "Comment: " <> str
    Identifier(str) -> "Ident: " <> str
    Whitespace(str) -> "Whitespace: '" <> str <> "'"
  }
}

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
  Whitespace(String)
  String(String)
  Number(Int)
  Comment(String)
  Identifier(String)
}