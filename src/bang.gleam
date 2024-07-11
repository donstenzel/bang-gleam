// import gleam/erlang.{get_line as input}
import gleam/int
import gleam/io
import gleam/list
import gleam/string

pub fn main() {
  //input("...")

  case string.pop_grapheme("blablabla") {
    Ok(#(grapheme, _)) -> {
      case grapheme {
            "a" | "A" | "b" | "B" | "c" | "C" | "d" | "D"
          | "e" | "E" | "f" | "F" | "g" | "G" | "h" | "H"
          | "i" | "I" | "j" | "J" | "k" | "K" | "l" | "L"
          | "m" | "M" | "n" | "N" | "o" | "O" | "p" | "P"
          | "q" | "Q" | "r" | "R" | "s" | "S" | "t" | "T"
          | "u" | "U" | "v" | "U" | "w" | "W" | "x" | "X"
          | "y" | "Y" | "z" | "Z" | "_" ->
              io.println("Alphabetical bro!")
          _ -> io.println("Nuh uh!!")
      }
    }
    Error(_) -> panic
  }

  let #(eaten, rest) =
    eat(#([], "128374783  no number anymore"), fn(str) {
      string.contains("0987654321", str)
    })

  io.println(rest)

  case list.reduce(eaten, fn(s1, s2) { s2 <> s1 }) {
    Ok(str) -> io.println(str)

    Error(_) -> Nil
  }

  let tokens = parse("  123  'string'+++")

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
  let eat_str_content = eat(_, fn(chr) { chr != "'" })
  let eat_string = fn(state: #(List(String), String)) {
    let #(_, rest) = state
    case string.pop_grapheme(rest) {
      Ok(#(grapheme, rest)) ->
        case grapheme {
          "'" -> {
            let #(parsed, rest) = eat_str_content(#([], rest))
            case string.pop_grapheme(rest) {
              Ok(#(grapheme, rest)) ->
                case grapheme {
                  "'" -> {
                    let quotestr = ["'", ..parsed]
                    let quotertsetouq = ["'", ..list.reverse(quotestr)]
                    #(list.reverse(quotertsetouq), rest)
                  }
                  _ -> panic
                }
              Error(_) -> panic
            }
          }
          _ -> panic
        }
      Error(_) -> panic
    }
  }

  case string.pop_grapheme(str) {
    Ok(#(grapheme, rest)) ->
      case grapheme {
        "+" -> [Plus, ..parse(rest)]

        "-" -> [Minus, ..parse(rest)]

        " " | "\t" | "\n" -> {
          let #(parsed, rest) = eat_whitespace(#([], str))
          [Whitespace(collapse(parsed)), ..parse(rest)]
        }

        "'" -> {
          let #(parsed, rest) = eat_string(#([], str))
          [String(collapse(parsed)), ..parse(rest)]
        }

        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> {
          let #(parsed, rest) = eat_number(#([], str))
          case int.parse(collapse(parsed)) {
            Ok(num) -> [Number(num), ..parse(rest)]
            Error(_) ->
              panic as "Number must parse when starting digit is found."
          }
        }

        _ -> []
      }

    Error(_) -> []
  }
}

// pub type LinkedList(t) {
//   Cons(t, LinkedList(t))
//   Nil
// }

// pub type Representable {
//   Grapheme(String)
//   Token(Token)
//   LinkedList(LinkedList(Representable))
// }

// pub fn represent(val: Representable) -> String {
//   case val {
//     Token(t) -> repr(t)
//     LinkedList(ll) ->
//       case ll {
//         Nil -> "~"
//         Cons(val, rest) -> represent(val) <> represent(LinkedList(rest))
//       }
//     Grapheme(str) -> str
//   }
// }

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
    TTrue -> "true"
    TFalse -> "false"
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
  // the relevant string should be obvious. mostly just the name in lowercase, exceptions are commented.
  // end of file        
  END

  Whitespace(String)

  // ()
  Leftparen
  Rightparen
  // []
  Leftbracket
  Rightbracket
  // {}
  Leftbrace
  Rightbrace

  // Single Character Operators
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

  // Double Character Operators
  // ==
  Dequals
  Lessequals
  Greaterequals
  Notequals
  Leftshift
  Rightshift
  // |>
  Pipe
  // ->
  Bind

  // Literals
  // (' | ") + content + (' | ") <- quotes have to match.
  String(String)
  // some number of numeric chars in a row, for now just ints. but simple to fix - just copy the float parser from jsonLexer
  Number(Int)
  // //... until end of line
  Comment(String)
  // [a-zA-Z0-9_]+ that doesnt match any keyword
  Identifier(String)

  // Keywords
  // var
  Variable
  // val
  Value
  // fun
  Function
  Return
  TTrue
  TFalse
  If
  Else
  While
  For
  In
  Match
  Area
  // struct
  Structure
  // enum
  Enumeration
  This
  Extend
  Extension
  Like
  // you can organize methods and values in areas. areas are merged at compile time and will
  // error if name overlaps
  // extend T { ... } adds methods to struct or enum
  // extension = abstract set of methods for polymorphism
  // extension A { fun a(this, ...) ... }
  // extend T like A { fun a(this, ...) { ... } ...}
}
// pub fn take_rec(
//   str: String,
//   pred: fn(String) -> Bool,
// ) -> #(LinkedList(Representable), String) {
//   case string.pop_grapheme(str) {
//     Ok(#(grapheme, rest)) -> {
//       case pred(grapheme) {
//         True ->
//           case take_rec(rest, pred) {
//             #(curr, rest_n) -> #(Cons(Grapheme(grapheme), curr), rest_n)
//           }

//         False -> #(Nil, rest)
//       }
//     }
//     _ -> #(Nil, "")
//   }
// }

// pub fn tokenize(source: String) -> LinkedList(Representable) {
//   let take_ident = take_rec(_, fn(char: String) {
//     string.contains(
//       "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
//       char,
//     )
//   })

//   let handle_ident = fn(str: String) {
//     case take_ident(str) {
//       #(ident, rest) ->
//         Cons(Token(Identifier(represent(LinkedList(ident)))), tokenize(rest))
//     }
//   }

//   let take_number = take_rec(_, fn(char: String) {
//     string.contains("_0123456789", char)
//   })

//   let handle_num = fn(str: String) {
//     case take_number(str) {
//       #(num, rest) ->
//         case int.parse(represent(LinkedList(num))) {
//           Ok(n) -> Cons(Token(Number(n)), tokenize(rest))
//           _ -> Nil
//         }
//     }
//   }

//   let take_string = take_rec(_, fn(char: String) { char != "'" })

//   let handle_string = fn(str: String) {
//     case take_string(str) {
//       #(content, rest) ->
//         case rest {
//           "'" <> rest ->
//             Cons(Token(String(represent(LinkedList(content)))), tokenize(rest))
//           _ -> Nil
//         }
//     }
//   }

//   case source {
//     "(" <> rest -> Cons(Token(Leftparen), tokenize(rest))
//     ")" <> rest -> Cons(Token(Rightparen), tokenize(rest))
//     "[" <> rest -> Cons(Token(Leftbracket), tokenize(rest))
//     "]" <> rest -> Cons(Token(Rightbracket), tokenize(rest))
//     "{" <> rest -> Cons(Token(Leftbrace), tokenize(rest))
//     "}" <> rest -> Cons(Token(Rightbrace), tokenize(rest))

//     other ->
//       case string.pop_grapheme(other) {
//         Ok(#(grapheme, rest)) ->
//           case grapheme {
//             "'" -> handle_string(rest)
//             "_" ->
//               case string.pop_grapheme(rest) {
//                 Ok(#(grapheme, _)) ->
//                   case grapheme {
//                     "a"
//                     | "A"
//                     | "b"
//                     | "B"
//                     | "c"
//                     | "C"
//                     | "d"
//                     | "D"
//                     | "e"
//                     | "E"
//                     | "f"
//                     | "F"
//                     | "g"
//                     | "G"
//                     | "h"
//                     | "H"
//                     | "i"
//                     | "I"
//                     | "j"
//                     | "J"
//                     | "k"
//                     | "K"
//                     | "l"
//                     | "L"
//                     | "m"
//                     | "M"
//                     | "n"
//                     | "N"
//                     | "o"
//                     | "O"
//                     | "p"
//                     | "P"
//                     | "q"
//                     | "Q"
//                     | "r"
//                     | "R"
//                     | "s"
//                     | "S"
//                     | "t"
//                     | "T"
//                     | "u"
//                     | "U"
//                     | "v"
//                     | "U"
//                     | "w"
//                     | "W"
//                     | "x"
//                     | "X"
//                     | "y"
//                     | "Y"
//                     | "z"
//                     | "Z" -> handle_ident(other)
//                     "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
//                       handle_num(other)
//                     _ -> Nil
//                   }
//                 _ -> Nil
//               }
//             "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
//               handle_num(other)
//             "a"
//             | "A"
//             | "b"
//             | "B"
//             | "c"
//             | "C"
//             | "d"
//             | "D"
//             | "e"
//             | "E"
//             | "f"
//             | "F"
//             | "g"
//             | "G"
//             | "h"
//             | "H"
//             | "i"
//             | "I"
//             | "j"
//             | "J"
//             | "k"
//             | "K"
//             | "l"
//             | "L"
//             | "m"
//             | "M"
//             | "n"
//             | "N"
//             | "o"
//             | "O"
//             | "p"
//             | "P"
//             | "q"
//             | "Q"
//             | "r"
//             | "R"
//             | "s"
//             | "S"
//             | "t"
//             | "T"
//             | "u"
//             | "U"
//             | "v"
//             | "U"
//             | "w"
//             | "W"
//             | "x"
//             | "X"
//             | "y"
//             | "Y"
//             | "z"
//             | "Z" -> handle_ident(other)
//             _ -> Nil
//           }
//         _ -> Nil
//       }
//   }
// }
