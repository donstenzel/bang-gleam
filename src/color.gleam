import gleam/int
import gleam/result

pub const escape = "\u{001b}"

pub const off = "\u{001b}[0m"

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
