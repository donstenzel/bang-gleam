import ast
import token

import parzerker.{EState, e_if, e_map}

pub fn init_state_token(tkns) {
  EState(tkns, 0)
}

pub fn e_number(state: parzerker.State(token.Token)) {
  state
  |> {
    e_if(
      fn(t) {
        case t {
          token.Number(_) -> True
          _ -> False
        }
      },
      False,
      "Number",
    )
    |> e_map(fn(t) {
      let assert token.Number(n) = t
      ast.Number(n)
    })
  }
}

pub fn e_string(state: parzerker.State(token.Token)) {
  state
  |> {
    e_if(
      fn(t) {
        case t {
          token.String(_) -> True
          _ -> False
        }
      },
      False,
      "String",
    )
    |> e_map(fn(t) {
      let assert token.String(s) = t
      ast.String(s)
    })
  }
}
