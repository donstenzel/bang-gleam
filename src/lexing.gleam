import gleam/io
import gleam/list
import gleam/string
import lib.{collapse_lr, escape, safe_parse}
import parzerker.{
  EFailure, EState, ESuccess, e_cont0, e_cont1, e_if, e_map, e_map_err, e_map_if,
  e_or, e_seq_many, e_surr, e_until,
}
import token

pub fn lexing_test() {
  let bang = e_string("bang", False) |> e_map(token.String)
  #("bald", "bang")
  |> { init_state_str |> lib.both() }
  |> { bang |> lib.both() }
  |> io.debug()
}

pub fn init_state_str(str) {
  parzerker.EState(string.to_graphemes(str), 0)
}

pub fn e_char(
  char: String,
  fatal: Bool,
) -> parzerker.EFunction(String, String, List(String)) {
  e_if(fn(c) { c == char }, fatal, "'" <> escape(char) <> "'")
}

pub fn e_string(
  str: String,
  fatal: Bool,
) -> parzerker.EFunction(String, String, List(String)) {
  case string.to_graphemes(str) {
    [head, ..tail] -> {
      let e_head = e_char(head, fatal)
      let es_tail = list.map(tail, with: e_char(_, fatal))
      e_seq_many([e_head, ..es_tail], string.append)
      |> e_map_err(fn(err) {
        list.map(err, fn(s) { s <> " in '" <> str <> "'" })
      })
      // list.fold(from: e_head, over: tail, with: fn(e, char) {
      //   e_seq(e, e_char(char, fatal), string.append)
      // })
    }
    [] -> panic as "Cannot eat empty string."
  }
}

pub fn e_const_token(
  str: String,
  fatal: Bool,
  out: token.Token,
) -> parzerker.EFunction(String, token.Token, List(String)) {
  e_string(str, fatal) |> e_map(fn(_) { out })
}

const keywords = [
  "var", "val", "fun", "return", "true", "false", "if", "else", "while", "for",
  "in", "match", "area", "struct", "enum", "this", "extend", "extension", "like",
]

pub fn e_end() {
  fn(state: parzerker.State(i)) {
    let EState(rest, tally) = state

    case rest {
      [] -> ESuccess(EState([], tally), token.END)
      _ -> EFailure(tally, ["Expected empty input."], False)
    }
  }
}

pub fn lex(
  state: parzerker.State(String),
) -> parzerker.EResult(String, List(token.Token), List(String)) {
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

  let e_cmmnt =
    parzerker.e_seq_r(
      e_char("#", False),
      parzerker.e_if(fn(chr) { chr != "\n" }, False, "not new-line")
        |> parzerker.e_cont0(),
    )
    |> parzerker.e_map(fn(str) {
      str
      |> collapse_lr()
      |> token.Comment()
    })

  let e_space = " " |> e_char(False)
  let e_tab = "\t" |> e_char(False)
  let e_newln = "\n" |> e_char(False)

  let e_whtspc =
    e_space
    |> e_or(e_tab, list.append)
    |> e_or(e_newln, list.append)
    |> e_cont1()
    |> e_map(fn(str) {
      str
      |> collapse_lr()
      |> escape()
      |> token.Whitespace()
    })
    |> e_map_err(fn(errs) {
      errs
      |> list.map(fn(err) { err <> " (Whitespace)" })
    })

  let e_quote = "'" |> e_char(False)
  let e_strcntnt =
    e_if(fn(c) { c != "'" }, False, "not '")
    |> e_cont0()
  let e_str =
    e_surr(e_quote, e_strcntnt, e_quote)
    |> e_map(fn(chrs) {
      chrs
      |> collapse_lr()
      |> token.String()
    })

  let e_number =
    e_if(fn(chr) { "0123456789" |> string.contains(chr) }, False, "numeric")
    |> e_cont1()
    |> e_map(fn(chrs) {
      chrs
      |> collapse_lr()
      |> safe_parse()
      |> token.Number()
    })

  let e_ident =
    e_if(
      fn(chr) {
        "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        |> string.contains(chr)
      },
      False,
      "alphabetic & _",
    )
    |> e_cont1()
    |> e_map_if(
      fn(chrs) {
        let str = chrs |> collapse_lr()
        !list.contains(keywords, str)
      },
      "Identifier cannot be a keyword.",
    )
    |> e_map(fn(chrs) {
      chrs
      |> collapse_lr()
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

  state
  |> {
    case list.reduce(over: es, with: fn(l, r) { e_or(l, r, list.append) }) {
      Ok(p) -> p
      Error(_) -> panic as "this should work!"
    }
    |> e_until(e_end())
  }
}
