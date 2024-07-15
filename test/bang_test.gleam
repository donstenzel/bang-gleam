import gleam/list
import gleeunit
import gleeunit/should
import bang

pub fn main() {
  gleeunit.main()
}

pub fn e_if_test() {
  let a = bang.e_if(fn(chr) { chr == "A" }, False, "'A'")
  "A" |> bang.init_state_str()
      |> a()
      |> should.equal(bang.ESuccess(bang.EState([], 1), "A"))

  "B" |> bang.init_state_str()
      |> a()
      |> should.equal(bang.EFailure(bang.EState(["B"], 0), ["Input @0 >  B did not match: 'A'"], False))
}

pub fn e_char_test() {
  let a = bang.e_char("A", False)
  "A" |> bang.init_state_str()
      |> a()
      |> bang.is_success()
      |> should.be_true()

  "B" |> bang.init_state_str()
      |> a()
      |> bang.is_error()
      |> should.be_true()
}

pub fn e_string_test() {
  let bang = bang.e_string("bang", False)
  "bang" |> bang.init_state_str()
         |> bang()
         |> should.equal(bang.ESuccess(bang.EState([], 4), "bang"))
  
  "bang and more"
    |> bang.init_state_str()
    |> bang()
    |> should.equal(
      bang.ESuccess(
        bang.EState([" ", "a", "n", "d", " ", "m", "o", "r", "e"], 4),
        "bang"
      )
    )

  "bald" |> bang.init_state_str()
         |> bang()
         |> bang.fork(
          fn(r) {
            r |> bang.is_tally(2)
              |> should.be_true()
          },
          fn(r) {
            r |> bang.is_error()
              |> should.be_true()
          })
}
// Combinators:

pub fn e_or_test() {
  let a = bang.e_char("A", False)
  let b = bang.e_char("B", False)

  let a_or_b = bang.e_or(a, b, list.append)

  "A" |> bang.init_state_str()
      |> a_or_b()
      |> should.equal(bang.ESuccess(bang.EState([], 1), "A"))

  "B" |> bang.init_state_str()
      |> a_or_b()
      |> should.equal(bang.ESuccess(bang.EState([], 1), "B"))

  "C" |> bang.init_state_str()
      |> a_or_b()
      |> bang.is_error()
      |> should.be_true()
}