import gleam/erlang.{ get_line as input }
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleam/order
import simplifile
import argv

pub fn main() {
  let bang = e_string("bang", False) |> e_map(String)
  "bald" |> init_state()
         |> bang()
         |> io.debug()

  startup()
  }

pub fn safe_parse(str) -> Int {
  case int.parse(str) {
    Ok(num) -> num
    Error(_) -> panic as "Input has to be pre-validated."
  }
}

pub fn startup() {


  case argv.load().arguments {
    [] -> start_repl(parse)
    [path] ->
      case simplifile.read(path) {
        Ok(source) -> case source |> init_state |> parse() {
           ESuccess(_, tokens) -> tokens |> repr_tokens() |> io.println()
           EFailure(_, errors, fatal) -> error_str(errors, fatal) |> io.println()
        }
        Error(e) -> { "Could not read file: " <> path <> "\n" <> simplifile.describe_error(e) } |> io.println()
      }
    _ -> io.println("Usage:\n - './bang' -> repl\n - './bang <path>' -> file")
  }
}

pub fn start_repl(parse) {
  io.print("Welcome to the bang! interactive environment. for now only tokenizes your input\n\n")
  repl(parse)
}

pub fn repl(parse: fn(EState) -> EResult(List(Token), List(String))) {
  case input("|> ") {
    Ok(str) -> case str |> init_state() |> parse() {
      ESuccess(_, tokens) -> tokens |> repr_tokens() |> io.println()
      EFailure(_, errors, fatal) -> error_str(errors, fatal) |> io.println()
    }
    Error(_) -> io.println("Couldn't get line.")
  }
  repl(parse)
}


pub fn error_str(errors, fatal) {
  let heading = case fatal {
    True -> "A fatal error has occured:"
    False -> "An error has occured:"
  }

  list.fold(
    from: heading,
    over: errors,
    with: fn(curr, e) {
      curr <> "\n - " <> e
    }
  )
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


pub fn parse(state) {
    let e_lparen = e_const_token("(", False, Leftparen)
  let e_rparen = e_const_token(")", False, Rightparen)
  let e_lbrckt = e_const_token("[", False, Leftbracket)
  let e_rbrckt = e_const_token("]", False, Rightbracket)
  let e_lbrace = e_const_token("{", False, Leftbrace)
  let e_rbrace = e_const_token("}", False, Rightbrace)
  let e_plus = e_const_token("+", False, Plus)
  let e_minus = e_const_token("-", False, Minus)
  let e_star = e_const_token("*", False, Star)
  let e_slash = e_const_token("/", False, Slash)
  let e_prcnt = e_const_token("%", False, Percent)
  let e_and = e_const_token("&", False, And)
  let e_ort = e_const_token("|", False, Or)
  let e_less = e_const_token("<", False, Less)
  let e_great = e_const_token(">", False, Greater)
  let e_equals = e_const_token("=", False, Equals)
  let e_bang = e_const_token("!", False, Bang)
  let e_dot = e_const_token(".", False, Dot)
  let e_qstmrk = e_const_token("?", False, Questionmark)
  let e_comma = e_const_token(",", False, Comma)
  let e_colon = e_const_token(":", False, Colon)
  let e_tilde = e_const_token("~", False, Tilde)
  let e_dequal = e_const_token("==", False, Dequals)
  let e_lteq = e_const_token("<=", False, Lessequals)
  let e_gteq = e_const_token(">=", False, Greaterequals)
  let e_nequal = e_const_token("!=", False, Notequals)
  let e_lshift = e_const_token("<<", False, Leftshift)
  let e_rshift = e_const_token(">>", False, Rightshift)
  let e_pipe = e_const_token("|>", False, Pipe)
  let e_bind = e_const_token("->", False, Bind)
  let e_var = e_const_token("var", False, Variable)
  let e_val = e_const_token("val", False, Value)
  let e_fun = e_const_token("fun", False, Function)
  let e_ret = e_const_token("return", False, Return)
  let e_true = e_const_token("true", False, Tru)
  let e_false = e_const_token("false", False, Fals)
  let e_ift = e_const_token("if", False, If)
  let e_else = e_const_token("else", False, Else)
  let e_while = e_const_token("while", False, While)
  let e_for = e_const_token("for", False, For)
  let e_in = e_const_token("in", False, In)
  let e_match = e_const_token("match", False, Match)
  let e_area = e_const_token("area", False, Area)
  let e_struct = e_const_token("struct", False, Structure)
  let e_enum = e_const_token("enum", False, Enumeration)
  let e_this = e_const_token("this", False, This)
  let e_xtend = e_const_token("extend", False, Extend)
  let e_xtnsn = e_const_token("extension", False, Extension)
  let e_like = e_const_token("like", False, Like)

  let e_cmmnt = e_seq(
    e_char("#", False),
    e_if(fn(chr) { chr != "\n" }, False, "not new-line") |> e_cont0(),
    fn(_, cntnt) { cntnt }
  ) |> e_map(fn(str) {
    str |> collapse_lr()
        |> Comment()
  })

  let e_space = " " |> e_char(False)
  let e_tab = "\t" |> e_char(False)
  let e_newln = "\n" |> e_char(False)

  let e_whtspc = e_space
  |> e_or(e_tab, list.append) 
  |> e_or(e_newln, list.append)
  |> e_cont1()
  |> e_map(fn(str) {
    str |> collapse_lr()
        |> escape()
        |> Whitespace()
  })

  state
  |> {
    case list.reduce(
      over: [
        e_whtspc,
        e_cmmnt,
        e_like,
        e_xtnsn,
        e_xtend,
        e_this,
        e_enum,
        e_struct,
        e_lparen,
        e_rparen,
        e_lbrckt,
        e_rbrckt,
        e_lbrace,
        e_rbrace,
        e_plus,
        e_minus,
        e_star,
        e_slash,
        e_prcnt,
        e_and,
        e_ort,
        e_less,
        e_great,
        e_equals,
        e_bang,
        e_dot,
        e_qstmrk,
        e_comma,
        e_colon,
        e_tilde,
        e_dequal,
        e_lteq,
        e_gteq,
        e_nequal,
        e_lshift,
        e_rshift,
        e_pipe,
        e_bind,
        e_var,
        e_val,
        e_fun,
        e_ret,
        e_true,
        e_false,
        e_ift,
        e_else,
        e_while,
        e_for,
        e_in,
        e_match,
        e_area,
      ],
      with: fn(l, r) { e_or(l, r, list.append) }
    ) {
      Ok(p) -> p
      Error(_) -> panic as "this should work!"
    } |> e_cont1() }
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
  e1: EFunction(s1, e),
  e2: EFunction(s2, e),
  combine_eaten: fn(s1, s2) -> s_new
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

pub fn e_seq_l(
  e1: EFunction(s, e),
  e2: EFunction(_, e)
) -> EFunction(s, e) {
  e_seq(e1, e2, fn(l, _) { l })
} 

pub fn e_seq_r(
  e1: EFunction(_, e),
  e2: EFunction(s, e)
) -> EFunction(s, e) {
  e_seq(e1, e2, fn(_, r) { r })
}

pub fn e_seq_many(
  es: List(EFunction(s, e)),
  combine_eaten: fn(s, s) -> s
) -> EFunction(s, e) {
  case es {
    [head, ..tail] -> {
      list.fold(from: head, over: tail, with: fn(e1, e2) {
        e_seq(e1, e2, combine_eaten)
      })
    }
    [] -> panic as "Cannot sequence 0 EFunctions."
  }
}

pub fn e_or(
  e1: EFunction(s, e),
  e2: EFunction(s, e),
  combine_errors: fn(e, e) -> e
) -> EFunction(s, e) {
  fn(state: EState) -> EResult(s, e) {
    case e1(state), e2(state) {

      ESuccess(EState(_, tally1), _) as s1, ESuccess(EState(_, tally2), _) as s2 -> {
        case int.compare(tally1, tally2) {
          order.Lt -> s2
          order.Eq | // <- maybe return error because of ambiguous match
          order.Gt -> s1
        }
      }

      EFailure(_, _, _), ESuccess(_, _) as s |
      ESuccess(_, _) as s, EFailure(_, _, _) -> s


      EFailure(EState(_, tally1) as old, error1, fatal1) as f1,
      EFailure(EState(_, tally2), error2, fatal2) as f2 -> {
        case int.compare(tally1, tally2) {
          order.Lt -> f2
          order.Gt -> f1
          order.Eq -> EFailure(
            old,
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
    EFailure(old, errors, fatal) -> ESuccess(state, [])
      // case fatal {
      //   False -> ESuccess(state, [])
      //   True -> EFailure(old, errors, fatal) // bubble up fatal errors.
      // }
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
    EFailure(old, errors, fatal) -> EFailure(old, errors, fatal)
      // case fatal {
      //   False -> ESuccess(state, [])
      //   True -> EFailure(old, errors, fatal) // bubble up fatal errors.
      // }
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

pub fn e_end() {
  fn(state: EState) {
    let EState(rest, tally) = state

    case rest {
      [] -> ESuccess(EState([], tally +1), END)
      _ -> EFailure(state, ["Expected empty input."], False)
    }
  }
}

pub fn e_if(
  predicate: fn(String) -> Bool,
  fatal: Bool,
  description: String // describes the predicate
) -> EFunction(String, List(String)) {
  fn(state: EState) -> EResult(String, List(String)) {
    let EState(rest, tally) = state

    case rest {
      [head, ..tail] ->
        case predicate(head) {
          True -> ESuccess(EState(tail, tally +1), head)
          False -> EFailure(
            state,
            [ "Input @"
            <> int.to_string(tally)
            <> " >  "
            <> escape(head)
            <> " did not match: "
            <> description ],
            fatal)
        }
      [] -> EFailure(EState([], tally), ["No input left to match."], True)
    }
  }
}

pub fn e_char(char: String, fatal: Bool) -> EFunction(String, List(String)) {
  e_if(fn(c) { c == char }, fatal, "'" <> char <> "'")
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

pub fn e_const_token(str: String, fatal: Bool, out: Token) -> EFunction(Token, List(String)) {
  e_string(str, fatal) |> e_map(fn(_) { out })
}