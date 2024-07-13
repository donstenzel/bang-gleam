import gleam/erlang.{ get_line as input }
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleam/order
import simplifile
import argv

pub fn main() {
  let bang = e_string("bang", False)
  "bald" |> init_state()
         |> bang()
         |> io.debug()

  //startup()
  }

pub fn safe_parse(str) -> Int {
  case int.parse(str) {
    Ok(num) -> num
    Error(_) -> panic as "Input has to be pre-validated."
  }
}

pub fn startup() {
  case argv.load().arguments {
    [] -> start_repl()
    [path] ->
      case simplifile.read(path) {
        Ok(source) -> source |> parse() |> repr_tokens() |> io.println()
        Error(e) -> { "Could not read file: " <> path <> "\n" <> simplifile.describe_error(e) } |> io.println()
      }
    _ -> io.println("Usage:\n - './bang' -> repl\n - './bang <path>' -> file")
  }
}

pub fn start_repl() {
  io.print("Welcome to the bang! interactive environment. for now only tokenizes your input\n\n")
  repl()
}

pub fn repl() {
  case input("|> ") {
    Ok(str) -> str |> parse() |> repr_tokens() |> io.println()
    Error(_) -> io.println("Couldn't get line.")
  }
  repl()
}


pub fn collapse_lr(str: List(String)) -> String {
  case list.reduce(str, fn(s1, s2) { s1 <> s2 }) {
    Ok(str) -> str
    Error(_) -> ""
  }
}

pub fn collapse_rl(str: List(String)) -> String {
  case list.reduce(str, fn(s1, s2) { s2 <> s1 }) {
    Ok(str) -> str
    Error(_) -> ""
  }
}

pub fn escape(str: String) {
  case string.pop_grapheme(str) {
    Ok(#(head, tail)) -> escape_chr(head) <> escape(tail)
    Error(_) -> ""
  }
}

pub fn escape_chr(chr: String) -> String {
  case chr {
    " " -> " "
    "\n" -> "\\n"
    "\t" -> "\\t"
    "\r" -> "\\r"
    "\\" -> "\\\\"
    regular -> regular
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
    #(parsed, rest)
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
          "'" -> #(parsed, rest)
          _ -> panic as "Can not happen."
        }
      Error(_) -> panic
    }
  }

  case str {
    "" -> [END]

    "("         <> rest -> [Leftparen,     ..parse(rest)]
    ")"         <> rest -> [Rightparen,    ..parse(rest)]
    "["         <> rest -> [Leftbracket,   ..parse(rest)]
    "]"         <> rest -> [Rightbracket,  ..parse(rest)]
    "{"         <> rest -> [Leftbrace,     ..parse(rest)]
    "}"         <> rest -> [Rightbrace,    ..parse(rest)]
    "+"         <> rest -> [Plus,          ..parse(rest)]
    "-"         <> rest -> [Minus,         ..parse(rest)]
    "*"         <> rest -> [Star,          ..parse(rest)]
    "/"         <> rest -> [Slash,         ..parse(rest)]
    "%"         <> rest -> [Percent,       ..parse(rest)]
    "&"         <> rest -> [And,           ..parse(rest)]
    "|"         <> rest -> [Or,            ..parse(rest)]
    "<"         <> rest -> [Less,          ..parse(rest)]
    ">"         <> rest -> [Greater,       ..parse(rest)]
    "="         <> rest -> [Equals,        ..parse(rest)]
    "!"         <> rest -> [Bang,          ..parse(rest)]
    "."         <> rest -> [Dot,           ..parse(rest)]
    "?"         <> rest -> [Questionmark,  ..parse(rest)]
    ","         <> rest -> [Comma,         ..parse(rest)]
    ":"         <> rest -> [Colon,         ..parse(rest)]
    "~"         <> rest -> [Tilde,         ..parse(rest)]
    "=="        <> rest -> [Dequals,       ..parse(rest)]
    "<="        <> rest -> [Lessequals,    ..parse(rest)]
    ">="        <> rest -> [Greaterequals, ..parse(rest)]
    "!="        <> rest -> [Notequals,     ..parse(rest)]
    "<<"        <> rest -> [Leftshift,     ..parse(rest)]
    ">>"        <> rest -> [Rightshift,    ..parse(rest)]
    "|>"        <> rest -> [Pipe,          ..parse(rest)]
    "->"        <> rest -> [Bind,          ..parse(rest)]
    "var"       <> rest -> [Variable,      ..parse(rest)]
    "val"       <> rest -> [Value,         ..parse(rest)]
    "fun"       <> rest -> [Function,      ..parse(rest)]
    "return"    <> rest -> [Return,        ..parse(rest)]
    "true"      <> rest -> [Tru,           ..parse(rest)]
    "false"     <> rest -> [Fals,          ..parse(rest)]
    "if"        <> rest -> [If,            ..parse(rest)]
    "else"      <> rest -> [Else,          ..parse(rest)]
    "while"     <> rest -> [While,         ..parse(rest)]
    "for"       <> rest -> [For,           ..parse(rest)]
    "in"        <> rest -> [In,            ..parse(rest)]
    "match"     <> rest -> [Match,         ..parse(rest)]
    "area"      <> rest -> [Area,          ..parse(rest)]
    "struct"    <> rest -> [Structure,     ..parse(rest)]
    "enum"      <> rest -> [Enumeration,   ..parse(rest)]
    "this"      <> rest -> [This,          ..parse(rest)]
    "extend"    <> rest -> [Extend,        ..parse(rest)]
    "extension" <> rest -> [Extension,     ..parse(rest)]
    "like"      <> rest -> [Like,          ..parse(rest)]
    
    // non constant patterns:
    _ -> {
      let assert Ok(#(grapheme, _)) = string.pop_grapheme(str) 
      case grapheme {
        "#" -> {
          let #(parsed, rest) = eat_comment(#([], str))
          [parsed |> collapse_rl() |> Comment, ..parse(rest)]
        }

        " " | "\t" | "\n" -> {
          let #(parsed, rest) = eat_whitespace(#([], str))
          [parsed |> collapse_rl() |> escape() |> Whitespace, ..parse(rest)]
        }

        "'" -> {
          let #(parsed, rest) = eat_string(#([], str))
          [parsed |> collapse_rl() |> String, ..parse(rest)]
        }

        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> {
          let #(parsed, rest) = eat_number(#([], str))
          let assert Ok(num) = parsed |> collapse_rl() |> int.parse()
          [num |> Number, ..parse(rest)]
        }

        "a" | "A" | "b" | "B" | "c" | "C" | "d" | "D" | "e" | "E" |
        "f" | "F" | "g" | "G" | "h" | "H" | "i" | "I" | "j" | "J" |
        "k" | "K" | "l" | "L" | "m" | "M" | "n" | "N" | "o" | "O" |
        "p" | "P" | "q" | "Q" | "r" | "R" | "s" | "S" | "t" | "T" |
        "u" | "U" | "v" | "U" | "w" | "W" | "x" | "X" | "y" | "Y" |
        "z" | "Z" | "_" -> {
          let #(parsed, rest) = eat_ident(#([], str))
          [parsed |> collapse_rl() |> Identifier, ..parse(rest)]
        }

        _ -> panic as "Unknown character."
      }
    }
  }
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



pub type EState {
  EState(rest: List(String), tally: Int)
}

pub fn init_state(str) {
  EState(string.to_graphemes(str), 0)
}

pub type EResult(s, e) {
  ESuccess(new: EState, eaten: s)
  EFailure(old: EState, error: e, fatal: Bool)
}

pub type EFunction(r, e) = fn(EState) -> EResult(r, e)

pub fn e_seq(
  e1: EFunction(s, e),
  e2: EFunction(s, e),
  combine_eaten: fn(s, s) -> s_new
) -> EFunction(s_new, e) {
  fn(state: EState) -> EResult(s_new, e) {
    case e1(state) {
      EFailure(state, error, fatal) -> EFailure(state, error, fatal)
      ESuccess(new, eaten1) -> 
        case e2(new) {
          EFailure(state, error, fatal) -> EFailure(state, error, fatal)
          ESuccess(new, eaten2) -> ESuccess(new, combine_eaten(eaten1, eaten2))
        }
    }
  }
}

pub fn e_or(
  e1: EFunction(s, e),
  e2: EFunction(s, e),
  combine_errors: fn(e, e) -> e
) -> EFunction(s, e) {
  fn(state: EState) -> EResult(s, e) {
    case e1(state), e2(state) {
      ESuccess(new1, _)as s1, ESuccess(new2, _) as s2 -> {
        let EState(_, tally1) = new1
        let EState(_, tally2) = new2
        case int.compare(tally1, tally2) {
          order.Lt -> s2
          order.Gt | // <- maybe return error because of ambiguous match
          order.Eq -> s1
        }
      }
      EFailure(_, _, _), ESuccess(_, _) as s |
      ESuccess(_, _) as s, EFailure(_, _, _) -> s
      EFailure(old1, error1, fatal1) as f1, EFailure(old2, error2, fatal2) as f2 -> {
        let EState(_, tally1) = old1
        let EState(_, tally2) = old2
        case int.compare(tally1, tally2) {
          order.Lt -> f2
          order.Gt -> f1
          order.Eq -> EFailure(
            old1,
            combine_errors(error1, error2),
            fatal1 || fatal2
          )
        }
      }
    }
  }
}

pub fn e_opt(
  e: EFunction(s, e),
  zero: s // the zero element under this operation over s.
) -> EFunction(s, e) {
  fn(state: EState) -> EResult(s, e) {
    case e(state) {
      EFailure(_, _, _) -> ESuccess(state, zero)
      ESuccess(_, _) as s -> s
    }
  }
}

pub fn e_cont0(e: EFunction(s, e)) -> EFunction(List(s), e) {
  e_cont0_rec(e, _)
}

pub fn e_cont0_rec(e: EFunction(s, e), state: EState) -> EResult(List(s), e) {
  case e(state) {
    ESuccess(new, eaten) ->
      case e_cont1_rec(e, new) {
        ESuccess(newer, eaten_tail) -> ESuccess(newer, [eaten, ..eaten_tail])
        EFailure(_, _, _) -> ESuccess(new, [eaten])
      }
    EFailure(_, _, _) -> ESuccess(state, [])
  }
}

pub fn e_cont1(e: EFunction(s, e)) -> EFunction(List(s), e) {
  e_cont1_rec(e, _)
}

pub fn e_cont1_rec(e: EFunction(s, e), state: EState) -> EResult(List(s), e) {
  case e(state) {
    ESuccess(new, eaten) ->
      case e_cont1_rec(e, new) {
        ESuccess(newer, eaten_tail) -> ESuccess(newer, [eaten, ..eaten_tail])
        EFailure(_, _, _) -> ESuccess(new, [eaten])
      }
    EFailure(old, error, fatal) -> EFailure(old, error, fatal)
  }
}


pub fn e_map(
  e: EFunction(s, e),
  f: fn(s) -> s_new
) -> EFunction(s_new, e) {
  fn(state: EState) -> EResult(s_new, e) {
    case e(state) {
      EFailure(state, error, fatal) -> EFailure(state, error, fatal)
      ESuccess(new, eaten) -> ESuccess(new, f(eaten))
    }
  }
}

pub fn e_if(
  predicate: fn(String) -> Bool,
  fatal: Bool
) -> EFunction(String, List(String)) {
  fn(state: EState) -> EResult(String, List(String)) {
    let EState(rest, tally) = state

    case rest {
      [head, ..tail] ->
        case predicate(head) {
          True -> ESuccess(EState(tail, tally +1), head)
          False -> EFailure(state, [head <> " did not match expected."], fatal)
        }
      [] -> EFailure(EState([], tally), ["No input left to match."], True)
    }
  }
}

pub fn e_char(char: String, fatal: Bool) -> EFunction(String, List(String)) {
  e_if(fn(c) { c == char }, fatal)
}

pub fn e_string(str: String, fatal: Bool) -> EFunction(String, List(String)) {
  case string.to_graphemes(str) {
    [head, ..tail] -> {
      let e_head = e_char(head, fatal)
      list.fold(from: e_head, over: tail, with: fn(e, char) {
        e_seq(e, e_char(char, fatal), string.append)
      })
    }
    [] -> panic as "Cannot eat empty string."
  }
}