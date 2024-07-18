import color
import gleam/int
import gleam/list
import gleam/string

pub fn safe_parse(str) -> Int {
  case int.parse(str) {
    Ok(num) -> num
    Error(_) -> panic as "Input has to be pre-validated."
  }
}

pub fn fork(f1: fn(a) -> b, f2: fn(a) -> c) -> fn(a) -> #(b, c) {
  fn(v: a) -> #(b, c) { #(f1(v), f2(v)) }
}

pub fn both(f: fn(a) -> b) -> fn(#(a, a)) -> #(b, b) {
  fn(v: #(a, a)) -> #(b, b) {
    let #(v1, v2) = v
    #(f(v1), f(v2))
  }
}

pub fn vectorize(f: fn(a) -> b) -> fn(List(a)) -> List(b) {
  vectorized(f, _)
}

fn vectorized(f, vs) {
  case vs {
    [head, ..tail] -> [f(head), ..vectorized(f, tail)]
    [] -> []
  }
}

pub fn error_str(errors, fatal) {
  let heading = case fatal {
    True -> color.error() <> "A fatal error has occured:"
    False -> color.error() <> "An error has occured:"
  }

  list.fold(from: heading, over: errors, with: fn(curr, e) {
    curr <> "\n - " <> e
  })
  <> color.off
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
    "\"" -> "'"
    "\n" -> "↲"
    "\t" -> "→"
    "\r" -> "←"
    "\\" -> "\\"
    regular -> regular
  }
}
