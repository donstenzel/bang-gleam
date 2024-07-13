import gleam/list
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

pub fn e_if_test() {
  let a = bang.e_if(fn(chr) { chr == "A" }, False)
  "A" |> bang.init_state()
      |> a()
      |> should.equal(bang.ESuccess(bang.EState([], 1), "A"))

  "B" |> bang.init_state()
      |> a()
      |> should.equal(bang.EFailure(bang.EState(["B"], 0), ["B did not match expected."], False))
}

// Combinators:

pub fn or_test() {
  let a = bang.e_if(fn(chr) { chr == "A" }, False)
  let b = bang.e_if(fn(chr) { chr == "B" }, False)

  let a_or_b = bang.e_or(a, b, list.append)

  "A" |> bang.init_state()
      |> a_or_b()
      |> should.equal(bang.ESuccess(bang.EState([], 1), "A"))

  "B" |> bang.init_state()
      |> a_or_b()
      |> should.equal(bang.ESuccess(bang.EState([], 1), "B"))

  "C" |> bang.init_state()
      |> a_or_b()
      |> should.equal(bang.EFailure(bang.EState(["C"], 0), ["C did not match expected.", "C did not match expected."], False))
}