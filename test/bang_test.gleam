import gleeunit
import gleeunit/should
import bang

pub fn main() {
  gleeunit.main()
}

pub fn whitespace_test() {
  " " |> bang.parse()
      |> should.equal([bang.Whitespace(" "), bang.END])
}

pub fn string_test() {
  "'This is a String, containing anything but single quotes'"
    |> bang.parse()
    |> should.equal([bang.String("This is a String, containing anything but single quotes"), bang.END])
}

pub fn integer_test() {
  "100000" |> bang.parse()
           |> should.equal([bang.Number(100000), bang.END])
}

pub fn comment_test() {
  "# This is a comment, ending on a newline.\n"
    |> bang.parse()
    |> should.equal([bang.Comment(" This is a comment, ending on a newline."), bang.Whitespace("\\n"), bang.END])
}
