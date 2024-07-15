import ast
import gleam/io
import gleam/list
import lib
import token

import parzerker

pub fn parsing_test() {
  [
    [token.Number(10)],
    [token.String("bang!")],
    [token.Tru],
    [token.Fals],
    [token.Identifier("variable")],
  ]
  |> { init_state_token |> lib.vectorize() }
  |> { e_literal |> lib.vectorize() }
  |> io.debug()
}

pub fn init_state_token(tkns) {
  parzerker.EState(tkns, 0)
}

pub fn e_number(state: parzerker.State(token.Token)) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Number(_) -> True
          _ -> False
        }
      },
      False,
      "Number",
    )
    |> parzerker.e_map(fn(t) {
      let assert token.Number(n) = t
      ast.Number(n)
    })
  }
}

pub fn e_string(state: parzerker.State(token.Token)) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.String(_) -> True
          _ -> False
        }
      },
      False,
      "String",
    )
    |> parzerker.e_map(fn(t) {
      let assert token.String(s) = t
      ast.String(s)
    })
  }
}

pub fn e_bool(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Tru | token.Fals -> True
          _ -> False
        }
      },
      False,
      "Boolean",
    )
    |> parzerker.e_map(fn(t) {
      case t {
        token.Tru -> ast.Boolean(True)
        token.Fals -> ast.Boolean(False)
        _ -> panic as "only Tru & Fals can get here."
      }
    })
  }
}

pub fn e_ref(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Identifier(_) -> True
          _ -> False
        }
      },
      False,
      "Identifier",
    )
    |> parzerker.e_map(fn(t) {
      let assert token.Identifier(id) = t
      ast.Reference(id)
    })
  }
}

pub fn e_literal(state) {
  state
  |> {
    e_number
    |> parzerker.e_or(e_string, list.append)
    |> parzerker.e_or(e_bool, list.append)
    |> parzerker.e_or(e_ref, list.append)
    |> parzerker.e_or(
      { parzerker.e_surr(e_lparen, e_expr, e_rparen) },
      list.append,
    )
  }
}

pub fn e_lparen(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Leftparen -> True
          _ -> False
        }
      },
      False,
      "Identifier",
    )
    |> parzerker.e_map(fn(_) { token.Leftparen })
  }
}

pub fn e_rparen(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Rightparen -> True
          _ -> False
        }
      },
      False,
      "Identifier",
    )
    |> parzerker.e_map(fn(_) { token.Rightparen })
  }
}

pub fn e_expr(state) {
  todo
}
