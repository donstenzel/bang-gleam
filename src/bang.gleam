import gleam/erlang.{ get_line as input }
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleam/order
import token
import simplifile
import argv

pub fn main() {
  let bang = e_string("bang", False) |> e_map(token.String)
  "bald" |> init_state_str()
         |> bang()
         |> io.debug()


  [token.Number(10)]
  |> init_state_token()
  |> e_number()
  |> string.inspect
  |> io.println()


  startup()

  }

pub fn startup() {
  case argv.load().arguments {
    [] -> {
      io.print("Welcome to the bang! interactive environment. for now only tokenizes your input\n\n")
      repl(parse)
    }
    [path] ->
      case simplifile.read(path) {
        Ok(source) -> case source |> init_state_str |> parse() {
           ESuccess(_, tokens) -> tokens |> token.repr_tokens() |> io.println()
           EFailure(_, errors, fatal) -> error_str(errors, fatal) |> io.println()
        }
        Error(e) -> { "Could not read file: " <> path <> "\n" <> simplifile.describe_error(e) } |> io.println()
      }
    _ -> io.println("Usage:\n - './bang' -> repl\n - './bang <path>' -> file")
  }
}

pub fn repl(parse: fn(State(String)) -> EResult(String, List(token.Token), List(String))) {
  case input("|> ") {
    Ok(str) -> case str |> init_state_str() |> parse() {
      ESuccess(_, tokens) -> tokens |> token.repr_tokens() |> io.println()
      EFailure(_, errors, fatal) -> error_str(errors, fatal) |> io.println()
    }
    Error(_) -> io.println("Couldn't get line.")
  }
  repl(parse)
}

pub fn safe_parse(str) -> Int {
  case int.parse(str) {
    Ok(num) -> num
    Error(_) -> panic as "Input has to be pre-validated."
  }
}

pub fn fork(f1: fn(v) -> a, f2: fn(v) -> b) -> fn(v) -> #(a, b) {
  fn(val: v) -> #(a, b) {
    #(f1(val), f2(val))
  }
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
    " " ->  " "
    "\n" -> "↲"
    "\t" -> "→"
    "\r" -> "←"
    "\\" -> "\\"
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

const keywords = [
  "var",
  "val",
  "fun",
  "return",
  "true",
  "false",
  "if",
  "else",
  "while",
  "for",
  "in",
  "match",
  "area",
  "struct",
  "enum",
  "this",
  "extend",
  "extension",
  "like"
]




pub fn parse(state: State(String)) -> EResult(String, List(token.Token), List(String)) {
  let e_lparen = e_const_token("(", False, token.Leftparen)
  let e_rparen = e_const_token(")", False, token.Rightparen)
  let e_lbrckt = e_const_token("[", False, token.Leftbracket)
  let e_rbrckt = e_const_token("]", False, token.Rightbracket)
  let e_lbrace = e_const_token("{", False, token.Leftbrace)
  let e_rbrace = e_const_token("}", False, token.Rightbrace)
  let e_plus = e_const_token("+", False, token.Plus)
  let e_minus = e_const_token("-", False, token.Minus)
  let e_star = e_const_token("*", False, token.Star)
  let e_slash = e_const_token("/", False, token.Slash)
  let e_prcnt = e_const_token("%", False, token.Percent)
  let e_and = e_const_token("&", False, token.And)
  let e_ort = e_const_token("|", False, token.Or)
  let e_less = e_const_token("<", False, token.Less)
  let e_great = e_const_token(">", False, token.Greater)
  let e_equals = e_const_token("=", False, token.Equals)
  let e_bang = e_const_token("!", False, token.Bang)
  let e_dot = e_const_token(".", False, token.Dot)
  let e_qstmrk = e_const_token("?", False, token.Questionmark)
  let e_comma = e_const_token(",", False, token.Comma)
  let e_colon = e_const_token(":", False, token.Colon)
  let e_tilde = e_const_token("~", False, token.Tilde)
  let e_dequal = e_const_token("==", False, token.Dequals)
  let e_lteq = e_const_token("<=", False, token.Lessequals)
  let e_gteq = e_const_token(">=", False, token.Greaterequals)
  let e_nequal = e_const_token("!=", False, token.Notequals)
  let e_lshift = e_const_token("<<", False, token.Leftshift)
  let e_rshift = e_const_token(">>", False, token.Rightshift)
  let e_pipe = e_const_token("|>", False, token.Pipe)
  let e_bind = e_const_token("->", False, token.Bind)
  let e_var = e_const_token("var", False, token.Variable)
  let e_val = e_const_token("val", False, token.Value)
  let e_fun = e_const_token("fun", False, token.Function)
  let e_ret = e_const_token("return", False, token.Return)
  let e_true = e_const_token("true", False, token.Tru)
  let e_false = e_const_token("false", False, token.Fals)
  let e_ift = e_const_token("if", False, token.If)
  let e_else = e_const_token("else", False, token.Else)
  let e_while = e_const_token("while", False, token.While)
  let e_for = e_const_token("for", False, token.For)
  let e_in = e_const_token("in", False, token.In)
  let e_match = e_const_token("match", False, token.Match)
  let e_area = e_const_token("area", False, token.Area)
  let e_struct = e_const_token("struct", False, token.Structure)
  let e_enum = e_const_token("enum", False, token.Enumeration)
  let e_this = e_const_token("this", False, token.This)
  let e_xtend = e_const_token("extend", False, token.Extend)
  let e_xtnsn = e_const_token("extension", False, token.Extension)
  let e_like = e_const_token("like", False, token.Like)

  let e_cmmnt = e_seq_r(
    e_char("#", False),
    e_if(fn(chr) { chr != "\n" }, False, "not new-line") |> e_cont0(),
    // fn(_, cntnt) { cntnt }
  ) |> e_map(fn(str) {
    str |> collapse_lr()
        |> token.Comment()
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
        |> token.Whitespace()
  })
  |> e_map_err(fn(errs) {
    errs |> list.map(fn(err) {
      err <> " (Whitespace)"
    })
  })

  let e_quote = "'" |> e_char(False)
  let e_strcntnt = e_if(fn(c) { c != "'" }, False, "not '")
                   |> e_cont0()
  let e_str = e_surr(e_quote, e_strcntnt, e_quote)
              |> e_map(fn(chrs) {
                chrs |> collapse_lr()
                     |> token.String()
              })

  let e_number = e_if(
    fn(chr) {
      "0123456789" |> string.contains(chr)
    },
    False,
    "numeric"
  ) |> e_cont1()
    |> e_map(fn(chrs) {
      chrs |> collapse_lr()
           |> safe_parse()
           |> token.Number()
    })

  let e_ident = e_if(
    fn(chr) {
      "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      |> string.contains(chr)
    },
    False,
    "alphabetic & _"
  ) |> e_cont1()
    |> e_map_if(
      fn(chrs) {
        let str = chrs |> collapse_lr()
        ! list.contains(keywords, str)
      },
      "Identifier cannot be a keyword."
    )
    |> e_map(
      fn(chrs) {
        chrs |> collapse_lr()
             |> token.Identifier()
      })

  let es = [
    e_ident,
    e_number,
    e_str,
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
  ]

  state |> {
    case list.reduce(
      over: es,
      with: fn(l, r) { e_or(l, r, list.append) }
    ) {
      Ok(p) -> p
      Error(_) -> panic as "this should work!"
    } |> e_until(e_end())
  }
}


pub type State(i) {
  EState(rest: List(i), tally: Int)
}

pub fn init_state_str(str) {
  EState(string.to_graphemes(str), 0)
}

pub type EResult(i, s, e) {
  ESuccess(new: State(i), eaten: s)
  EFailure(old: State(i), error: e, fatal: Bool)
}

pub fn is_success(r: EResult(_, _, _)) -> Bool {
  case r {
    ESuccess(_, _) -> True
    EFailure(_, _, _) -> False
  }
}

pub fn is_error(r: EResult(_, _, _)) -> Bool {
  case r {
    ESuccess(_, _) -> False
    EFailure(_, _, _) -> True
  }
}

pub fn is_tally(r: EResult(_, _, _), to_match) -> Bool {
  case r {
    ESuccess(EState(_, tally), _) -> tally == to_match
    EFailure(EState(_, tally), _, _) -> tally == to_match
  }
}

pub type EFunction(input, parsed, error) = fn(State(input)) -> EResult(input, parsed, error)

pub fn e_seq(
  e1: EFunction(i, s1, e),
  e2: EFunction(i, s2, e),
  combine_eaten: fn(s1, s2) -> s_new
) -> EFunction(i, s_new, e) {
  fn(state: State(i)) -> EResult(i, s_new, e) {
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
  e1: EFunction(i, s, e),
  e2: EFunction(i, _, e)
) -> EFunction(i, s, e) {
  e_seq(e1, e2, fn(l, _) { l })
} 

pub fn e_seq_r(
  e1: EFunction(i, _, e),
  e2: EFunction(i, s, e)
) -> EFunction(i, s, e) {
  e_seq(e1, e2, fn(_, r) { r })
}

pub fn e_surr(
  l: EFunction(i, _, e),
  e: EFunction(i, s, e),
  r: EFunction(i, _, e)
) -> EFunction(i, s, e) {
  e_seq_r(l, e_seq_l(e, r))
}

pub fn e_seq_many(
  es: List(EFunction(i, s, e)),
  combine_eaten: fn(s, s) -> s
) -> EFunction(i, s, e) {
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
  e1: EFunction(i, s, e),
  e2: EFunction(i, s, e),
  combine_errors: fn(e, e) -> e
) -> EFunction(i, s, e) {
  fn(state: State(i)) -> EResult(i, s, e) {
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
  e: EFunction(i, s, e),
  zero: s // the zero element under this operation over s.
) -> EFunction(i, s, e) {
  fn(state: State(i)) -> EResult(i, s, e) {
    case e(state) {
      EFailure(_, _, _) -> ESuccess(state, zero)
      ESuccess(_, _) as s -> s
    }
  }
}

pub fn e_cont0(e: EFunction(i, s, e)) -> EFunction(i, List(s), e) {
  e_cont0_rec(e, _)
}

pub fn e_cont0_rec(e: EFunction(i, s, e), state: State(i)) -> EResult(i, List(s), e) {
  case e(state) {
    ESuccess(new, eaten) ->
      case e_cont1_rec(e, new) {
        ESuccess(newer, eaten_tail) -> ESuccess(newer, [eaten, ..eaten_tail])
        EFailure(_, _, _) -> ESuccess(new, [eaten])
      }
    EFailure(_, _, _) -> ESuccess(state, [])
  }
}

pub fn e_cont1(e: EFunction(i, s, e)) -> EFunction(i, List(s), e) {
  e_cont1_rec(e, _)
}

pub fn e_cont1_rec(e: EFunction(i, s, e), state: State(i)) -> EResult(i, List(s), e) {
  case e(state) {
    ESuccess(new, eaten) ->
      case e_cont1_rec(e, new) {
        ESuccess(newer, eaten_tail) -> ESuccess(newer, [eaten, ..eaten_tail])
        EFailure(_, _, _) -> ESuccess(new, [eaten])
      }
    EFailure(old, errors, fatal) -> EFailure(old, errors, fatal)
  }
}


pub fn e_map(
  e: EFunction(i, s, e),
  f: fn(s) -> s_new
) -> EFunction(i, s_new, e) {
  fn(state: State(i)) -> EResult(i, s_new, e) {
    case e(state) {
      EFailure(state, error, fatal) -> EFailure(state, error, fatal)
      ESuccess(new, eaten) -> ESuccess(new, f(eaten))
    }
  }
}

pub fn e_map_if(
  e: EFunction(i, s, List(String)),
  p: fn(s) -> Bool,
  description: String
) -> EFunction(i, s, List(String)) {
  fn(state: State(i)) -> EResult(i, s, List(String)) {
    case e(state) {
      EFailure(_, _, _) as f -> f
      ESuccess(_, eaten) as s ->
        case p(eaten) {
          True -> s
          False -> EFailure(state, [description], False)
        }
    }
  }
}

pub fn e_map_err(
  e: EFunction(i, s, e),
  f: fn(e) -> e_new
) -> EFunction(i, s, e_new) {
  fn(state: State(i)) -> EResult(i, s, e_new) {
    case e(state) {
      ESuccess(new, eaten) -> ESuccess(new, eaten)
      EFailure(old, error, fatal) -> EFailure(old, f(error), fatal)
    }
  }
}

pub fn e_end() {
  fn(state: State(i)) {
    let EState(rest, tally) = state

    case rest {
      [] -> ESuccess(EState([], tally), token.END)
      _ -> EFailure(state, ["Expected empty input."], False)
    }
  }
}

pub fn e_until(
  e: EFunction(i, s, e),
  end: EFunction(i, s, e)
) -> EFunction(i, List(s), e) {
  e_until_inner(_, e, end)
}

pub fn e_until_inner(
  state: State(i),
  e: EFunction(i, s, e),
  end: EFunction(i, s, e)
) -> EResult(i, List(s), e) {
  case end(state) {
    EFailure(_, _, _) ->
      case e(state) {
        ESuccess(new, eaten) ->
          case e_until_inner(new, e, end) {
            ESuccess(newer, eaten_tail) -> {
              ESuccess(newer, [eaten, ..eaten_tail])
            }
            EFailure(old, errors, fatal) -> EFailure(old, errors, fatal)
          }
        EFailure(old, errors, fatal) -> EFailure(old, errors, fatal)
      }
    ESuccess(newer, eaten2) -> ESuccess(newer, [eaten2])
  }
}



pub fn e_if(
  predicate: fn(i) -> Bool,
  fatal: Bool,
  description: String // describes the predicate
) -> EFunction(i, i, List(String)) {
  fn(state: State(i)) -> EResult(i, i, List(String)) {
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
            <> { head |> string.inspect() }
            <> " did not match: "
            <> description ],
            fatal)
        }
      [] -> EFailure(EState([], tally), ["No input left to match."], True)
    }
  }
}

pub fn e_char(char: String, fatal: Bool) -> EFunction(String, String, List(String)) {
  e_if(fn(c) { c == char }, fatal, "'" <> escape(char) <> "'")
}

pub fn e_string(str: String, fatal: Bool) -> EFunction(String, String, List(String)) {
  case string.to_graphemes(str) {
    [head, ..tail] -> {
      let e_head = e_char(head, fatal)
      let es_tail = list.map(tail, with: e_char(_, fatal))
      e_seq_many([e_head, ..es_tail], string.append)
      |> e_map_err(fn(err) { list.map(err, fn(s) { s <> " in '" <> str <> "'" }) })
      // list.fold(from: e_head, over: tail, with: fn(e, char) {
      //   e_seq(e, e_char(char, fatal), string.append)
      // })
    }
    [] -> panic as "Cannot eat empty string."
  }
}

pub fn e_const_token(str: String, fatal: Bool, out: token.Token) -> EFunction(String, token.Token, List(String)) {
  e_string(str, fatal) |> e_map(fn(_) { out })
}


fn e_number(state: State(token.Token)) {
  state
  |> {
    e_if(fn(t) {
      case t {
        token.Number(_) -> True
        _ -> False
      }
    }, False, "Number")
    |> e_map(fn(t) {
      let assert token.Number(n) = t
      n
    })
  }
}

pub fn init_state_token(tkns) {
  EState(tkns, 0)
}