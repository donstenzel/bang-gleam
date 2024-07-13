import gleam/erlang.{ get_line as input }       
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleam/order
import simplifile
import argv

pub fn main() {
  let p = eat_if(matches: fn(chr) { string.contains("0123456789", chr) }, fatal: False)
            |> cont1()
            |> manipulate(fn(chrs) { chrs |> collapse() |> safe_parse() |> Number })
            |> cont0()

  let res = "123abc123  "
              |> new_state()
              |> p()
  
  io.debug(res)

  //startup()
  }

fn safe_parse(str) -> Int {
  case int.parse(str) {
    Ok(num) -> num
    Error(_) -> panic as "Input has to be pre-validated."
  }
}

fn startup() {
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

fn start_repl() {
  io.print("Welcome to the bang! interactive environment. for now only tokenizes your input\n\n")
  repl()
}

fn repl() {
  case input("|> ") {
    Ok(str) -> str |> parse() |> repr_tokens() |> io.println()
    Error(_) -> io.println("Couldn't get line.")
  }
  repl()
}


pub fn collapse(str: List(String)) -> String {
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



/// d -> digested item
/// f -> food item
/// r -> collapsed type
pub type State(d, f, r) {
  State(eaten: List(d), to_taste: List(f), tally: Int)
  Collapsed(res: r, to_taste: List(f), tally: Int)
}

pub fn new_state(str) {
  State([], string.to_graphemes(str), 0)
}

pub type Transition(d, f, r) {
  Success(new: State(d, f, r))
  Failure(offender: State(d, f, r), descs: List(String), fatal: Bool)
}

pub type Eat(d, f, r) = fn(State(d, f, r)) -> Transition(d, f, r)

pub fn manipulate(e: Eat(d, f, r), func: fn(List(d)) -> r) -> Eat(d, f, r) {
  fn(state: State(d, f, r)) -> Transition(d, f, r) {
    case e(state) {
      Success(state) -> {
        let assert State(eaten, to_taste, tally) = state
        Success(Collapsed(func(eaten), to_taste, tally))
      }
      Failure(_, _, _) as f -> f
    }
  }
}

pub fn seq(e1: Eat(d, f, r), e2: Eat(d, f, r)) -> Eat(d, f, r) {
  fn(state: State(d, f, r)) -> Transition(d, f, r) {
    case e1(state) {
      Success(state) -> 
        case e2(state) {
          Success(_) as s -> s
          Failure(_, _, _) as f -> f
        }
      Failure(_, _, _) as f -> f
    }
  }
}

// 0 or 1
pub fn opt(e: Eat(d, f, r)) -> Eat(d, f, r) {
  fn(state: State(d, f, r)) {
    case e(state) {
      Success(_) as s -> s
      Failure(_, _, _) -> Success(state)
    }
  }
}

// 0 or more
pub fn cont0(e: Eat(d, f, r)) -> Eat(d, f, r) {
  cont0_inner(_, e)
}

pub fn cont0_inner(state: State(d, f, r), e: Eat(d, f, r)) -> Transition(d, f, r) {
  case e(state) {
    Success(state) -> 
      case cont1_inner(state, e) {
        Success(_) as s -> s
        Failure(_, _, _) -> Success(state)
      }
    Failure(_, _, _) -> Success(state)
  }
}

// 1 or more
pub fn cont1(e: Eat(d, f, r)) -> Eat(d, f, r) {
  cont1_inner(_, e)
}

pub fn cont1_inner(state: State(d, f, r), e: Eat(d, f, r)) -> Transition(d, f, r) {
  case e(state) {
    Success(state) -> 
      case cont1_inner(state, e) {
        Success(_) as s -> s
        Failure(_, _, _) -> Success(state)
      }
    Failure(_, _, _) as f -> f
  }
}

pub fn combine_failure(f1: Transition(d, f, r), f2: Transition(d, f, r)) -> Transition(d, f, r) {
  let assert Failure(State(eaten1, to_taste1, tally1), descs1, fatal1) = f1
  let assert Failure(State(eaten2, to_taste2, tally2), descs2, fatal2) = f2
  case int.compare(tally1, tally2) {
    order.Lt -> Failure(State(eaten2, to_taste2, tally2), list.append(descs1, descs2), fatal1 || fatal2)
    order.Gt |
    order.Eq -> Failure(State(eaten1, to_taste1, tally1), list.append(descs1, descs2), fatal1 || fatal2)
  }
}

pub fn or(e1: Eat(d, f, r), e2: Eat(d, f, r)) -> Eat(d, f, r) { 
  fn(state: State(d, f, r)) -> Transition(d, f, r) {
    case e1(state) {
      Success(_) as s1 -> s1
      Failure(_, _, fatal) as f1 -> case fatal {
        True -> f1
        False -> {
          case e2(state) {
            Success(_) as s2 -> s2
            Failure(_, _, _) as f2 -> combine_failure(f1, f2)
          }
        }
      }
    }
  }
}


pub fn eat_if(matches predicate: fn(f) -> Bool, fatal fatal: Bool) -> Eat(f, f, r) {
  fn(state: State(f, f, r)) -> Transition(f, f, r) {
    // base cases: no input left, next char doesnt match
    let assert State(eaten, to_taste, tally) = state

    case to_taste {
      [head, ..tail] ->
        case predicate(head) {
          True -> Success(State([head, ..eaten], tail, tally +1))
          False -> Failure(state, ["Input did not match expected."], fatal)
        }
      [] -> Failure(state, ["No input left to eat @" <> tally |> int.to_string()], fatal)
    }
  }
}

pub fn eat_char(char: String, fatal: Bool) -> Eat(String, String, String) {
  eat_if(matches: fn(c) { c == char }, fatal: fatal)
}

pub fn eat_string(str: String, fatal: Bool) -> Eat(String, String, String) {
  case string.to_graphemes(str) {
    [head, ..tail] -> {
      let eat_head = eat_char(head, fatal)
      list.fold(from: eat_head, over: tail, with: fn(first, next_char) {
        seq(first, eat_char(next_char, fatal))
      } )
    }
    [] -> panic as "You can not eat an empty string!"
  }
}


pub fn parse_test(str: String) {
    case str {
    "" -> [END]

    "("         <> rest -> [Leftparen,     ..parse_test(rest)]
    ")"         <> rest -> [Rightparen,    ..parse_test(rest)]
    _ -> []
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
          [parsed |> collapse() |> Comment, ..parse(rest)]
        }

        " " | "\t" | "\n" -> {
          let #(parsed, rest) = eat_whitespace(#([], str))
          [parsed |> collapse() |> escape() |> Whitespace, ..parse(rest)]
        }

        "'" -> {
          let #(parsed, rest) = eat_string(#([], str))
          [parsed |> collapse() |> String, ..parse(rest)]
        }

        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> {
          let #(parsed, rest) = eat_number(#([], str))
          let assert Ok(num) = parsed |> collapse() |> int.parse()
          [num |> Number, ..parse(rest)]
        }

        "a" | "A" | "b" | "B" | "c" | "C" | "d" | "D" | "e" | "E" |
        "f" | "F" | "g" | "G" | "h" | "H" | "i" | "I" | "j" | "J" |
        "k" | "K" | "l" | "L" | "m" | "M" | "n" | "N" | "o" | "O" |
        "p" | "P" | "q" | "Q" | "r" | "R" | "s" | "S" | "t" | "T" |
        "u" | "U" | "v" | "U" | "w" | "W" | "x" | "X" | "y" | "Y" |
        "z" | "Z" | "_" -> {
          let #(parsed, rest) = eat_ident(#([], str))
          [parsed |> collapse() |> Identifier, ..parse(rest)]
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



type EState {
  EState(rest: List(String), tally: Int)
}

type EResult(s, e) {
  ESuccess(new: EState, eaten: s)
  EFailure(old: EState, error: e)
}

type EFunction(r, e) = fn(EState) -> EResult(r, e)

fn e_seq(
  e1: EFunction(s, e),
  e2: EFunction(s, e),
  combine_eaten: fn(s, s) -> s_new
) -> EFunction(s_new, e) {
  fn(state: EState) -> EResult(s_new, e) {
    case e1(state) {
      EFailure(state, error) -> EFailure(state, error)
      ESuccess(new, eaten1) -> 
        case e2(new) {
          EFailure(state, error) -> EFailure(state, error)
          ESuccess(new, eaten2) -> ESuccess(new, combine_eaten(eaten1, eaten2))
        }
    }
  }
}

fn e_or(
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
      EFailure(_, _), ESuccess(_, _) as s | ESuccess(_, _) as s, EFailure(_, _) -> s
      EFailure(old1, e1) as f1, EFailure(old2, e2) as f2 -> {
        let EState(_, tally1) = old1
        let EState(_, tally2) = old2
        case int.compare(tally1, tally2) {
          order.Lt -> f2
          order.Gt -> f1
          order.Eq -> EFailure(old1, combine_errors(e1, e2))
        }
      }

    }
  }
}

fn e_opt(
  e: EFunction(s, e),
  zero: s // the zero element under this operation over s.
) -> EFunction(s, e) {
  fn(state: EState) -> EResult(s, e) {
    case e(state) {
      EFailure(_, _) -> ESuccess(state, zero)
      ESuccess(_, _) as s -> s
    }
  }
}

fn e_map(
  e: EFunction(s, e),
  f: fn(s) -> s_new
) -> EFunction(s_new, e) {
  fn(state: EState) -> EResult(s_new, e) {
    case e(state) {
      EFailure(state, error) -> EFailure(state, error)
      ESuccess(new, eaten) -> ESuccess(new, f(eaten))
    }
  }
}