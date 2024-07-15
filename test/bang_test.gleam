import gleam/list
import gleeunit
import gleeunit/should
import parzerker
import lexing
import lib

pub fn main() {
  gleeunit.main()
}

pub fn e_if_test() {
  let a = parzerker.e_if(fn(chr) { chr == "A" }, False, "'A'")
  "A" |> lexing.init_state_str()
      |> a()
      |> should.equal(parzerker.ESuccess(parzerker.EState([], 1), "A"))

  "B" |> lexing.init_state_str()
      |> a()
      |> should.equal(parzerker.EFailure(parzerker.EState(["B"], 0), ["Input @0 >  'B' did not match: 'A'"], False))
}

pub fn e_char_test() {
  let a = lexing.e_char("A", False)
  "A" |> lexing.init_state_str()
      |> a()
      |> parzerker.is_success()
      |> should.be_true()

  "B" |> lexing.init_state_str()
      |> a()
      |> parzerker.is_error()
      |> should.be_true()
}

pub fn e_string_test() {
  let bang = lexing.e_string("bang", False)
  "bang" |> lexing.init_state_str()
         |> bang()
         |> should.equal(parzerker.ESuccess(parzerker.EState([], 4), "bang"))
  
  "bang and more"
    |> lexing.init_state_str()
    |> bang()
    |> should.equal(
      parzerker.ESuccess(
        parzerker.EState([" ", "a", "n", "d", " ", "m", "o", "r", "e"], 4),
        "bang"
      )
    )

  "bald" |> lexing.init_state_str()
         |> bang()
         |> lib.fork(
          fn(r) {
            r |> parzerker.is_tally(2)
              |> should.be_true()
          },
          fn(r) {
            r |> parzerker.is_error()
              |> should.be_true()
          })
}
// Combinators:

pub fn e_or_test() {
  let a = lexing.e_char("A", False)
  let b = lexing.e_char("B", False)

  let a_or_b = parzerker.e_or(a, b, list.append)

  "A" |> lexing.init_state_str()
      |> a_or_b()
      |> should.equal(parzerker.ESuccess(parzerker.EState([], 1), "A"))

  "B" |> lexing.init_state_str()
      |> a_or_b()
      |> should.equal(parzerker.ESuccess(parzerker.EState([], 1), "B"))

  "C" |> lexing.init_state_str()
      |> a_or_b()
      |> parzerker.is_error()
      |> should.be_true()
}