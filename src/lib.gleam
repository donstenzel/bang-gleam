import gleam/int
import gleam/string
import gleam/list

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
    "\"" -> "'"
    "\n" -> "↲"
    "\t" -> "→"
    "\r" -> "←"
    "\\" -> "\\"
    regular -> regular
  }
}
