import gleam/int
import gleam/list
import gleam/result

pub const escape = "\u{001b}"

pub const off = "\u{001b}[0m"

pub const invert = "\u{001b}[7m"

pub const invert_off = "\u{001b}[27m"

pub const arrow_sep_r = "\u{e0b0}"

pub const default_foreground = "\u{001b}[39m"

pub const default_background = "\u{001b}[49m"

pub fn seperated(colored_segments: List(#(#(Int, Int, Int), String))) -> String {
  // color1 <> element1 <> seperator(color1, color2) <> color2 <> ...
  case colored_segments {
    [head, ..tail] -> {
      let #(#(r1, g1, b1), segment1) = head

      let init =
        background(r1, g1, b1) <> " " <> segment1 <> foreground(r1, g1, b1)
      list.fold(from: init, over: tail, with: fn(curr, next) {
        let #(#(r2, g2, b2), segment2) = next
        curr
        <> " "
        <> background(r2, g2, b2)
        <> arrow_sep_r
        <> " "
        <> default_foreground
        <> segment2
        <> foreground(r2, g2, b2)
      })
      <> " "
      <> default_background
      <> arrow_sep_r
      <> off
    }
    [] -> ""
  }
}

fn in_range(v, b, e) {
  case b <= v && v <= e {
    True -> Ok(v)
    False ->
      Error(
        "Value "
        <> v |> int.to_string()
        <> " not in range ["
        <> b |> int.to_string()
        <> ", "
        <> e |> int.to_string()
        <> "]",
      )
  }
}

pub fn foreground(r, g, b) {
  escape
  <> "[38;2;"
  <> r |> int.to_string()
  <> ";"
  <> g |> int.to_string()
  <> ";"
  <> b |> int.to_string()
  <> "m"
}

pub fn background(r, g, b) {
  escape
  <> "[48;2;"
  <> r |> int.to_string()
  <> ";"
  <> g |> int.to_string()
  <> ";"
  <> b |> int.to_string()
  <> "m"
}

pub fn get_rgb_code(red: Int, green: Int, blue: Int) {
  let c_range = in_range(_, 0, 255)

  use valid_red <- result.try(c_range(red))
  use valid_green <- result.try(c_range(green))
  use valid_blue <- result.try(c_range(blue))

  Ok(
    escape
    <> "[38;2;"
    <> valid_red |> int.to_string()
    <> ";"
    <> valid_green |> int.to_string()
    <> ";"
    <> valid_blue |> int.to_string()
    <> "m",
  )
}

pub fn colored(str, color) {
  color <> str <> off
}

pub fn get_rgb_code_clamped(red, green, blue) {
  let assert Ok(str) =
    get_rgb_code(
      red |> int.clamp(0, 255),
      green |> int.clamp(0, 255),
      blue |> int.clamp(0, 255),
    )
  str
}

pub fn error() {
  get_rgb_code_clamped(150, 20, 20)
}

pub fn success() {
  get_rgb_code_clamped(22, 234, 98)
}

pub fn hint() {
  get_rgb_code_clamped(22, 234, 204)
}
