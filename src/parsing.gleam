import token
import ast

import parzerker.{
    e_if,
    e_map,
    EState
}

pub fn e_number(state: parzerker.State(token.Token)) {
  state
  |> {
    e_if(fn(t) {
      case t {
        token.Number(_) -> True
        _ -> False
      }
    }, False, "Number")
    |> e_map(fn(t) {
      let assert token.Number(n) = t
      ast.Number(n)
    })
  }
}

pub fn init_state_token(tkns) {
  EState(tkns, 0)
}