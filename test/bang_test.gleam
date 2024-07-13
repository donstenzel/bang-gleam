import gleam/list
import gleeunit
import gleeunit/should
import bang

pub fn main() {
  gleeunit.main()
}

pub fn e_if_test() {
  let a = bang.e_if(fn(chr) { chr == "A" }, False, "'A'")
  "A" |> bang.init_state()
      |> a()
      |> should.equal(bang.ESuccess(bang.EState([], 1), "A"))

  "B" |> bang.init_state()
      |> a()
      |> should.equal(bang.EFailure(bang.EState(["B"], 0), ["B did not match expected."], False))
}

pub fn e_char_test() {
  let a = bang.e_char("A", False)
  "A" |> bang.init_state()
      |> a()
      |> should.equal(bang.ESuccess(bang.EState([], 1), "A"))

  "B" |> bang.init_state()
      |> a()
      |> should.equal(bang.EFailure(bang.EState(["B"], 0), ["B did not match expected."], False))
}

pub fn e_string_test() {
  let bang = bang.e_string("bang", False)
  "bang" |> bang.init_state()
         |> bang()
         |> should.equal(bang.ESuccess(bang.EState([], 4), "bang"))
  
  "bang and more"
    |> bang.init_state()
    |> bang()
    |> should.equal(
      bang.ESuccess(
        bang.EState([" ", "a", "n", "d", " ", "m", "o", "r", "e"], 4),
        "bang"
      )
    )

  "bald" |> bang.init_state()
         |> bang()
         |> should.equal(bang.EFailure(bang.EState(["l", "d"], 2), ["l did not match expected."], False))
}
// Combinators:

pub fn e_or_test() {
  let a = bang.e_char("A", False)
  let b = bang.e_char("B", False)

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