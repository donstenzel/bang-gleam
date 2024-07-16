import ast
import gleam/io
import gleam/list
import token

import parzerker

pub fn parsing_test() {
  [
    token.Leftparen,
    token.Minus,
    token.Number(100),
    token.Rightparen,
    token.Bang,
  ]
  |> { init_state_token }
  |> { e_decl }
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

pub fn e_ident(state) {
  state
  |> parzerker.e_if(
    fn(t) {
      case t {
        token.Identifier(_) -> True
        _ -> False
      }
    },
    False,
    "Identifier",
  )
}

pub fn e_dot(state) {
  state
  |> parzerker.e_if(
    fn(t) {
      case t {
        token.Dot -> True
        _ -> False
      }
    },
    False,
    "Dot",
  )
}

fn climb_ref_rec(prev, rest) {
  case rest {
    [head, ..tail] -> climb_ref_rec(ast.Indirect(head, prev), tail)
    [] -> prev
  }
}

pub fn e_ref(state) {
  state
  |> {
    e_ident
    |> parzerker.e_seq(
      { e_dot |> parzerker.e_seq_r(e_ident) } |> parzerker.e_cont0,
      fn(head, tail) { [head, ..tail] },
    )
    |> parzerker.e_map(fn(ids) {
      let assert [direct, ..rest] = ids
      let rest =
        rest
        |> list.map(fn(t) {
          let assert token.Identifier(str) = t
          str
        })
      let assert token.Identifier(str) = direct
      climb_ref_rec(ast.Direct(str), rest) |> ast.Reference
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
      {
        parzerker.e_surr(
          e_lparen,
          { e_expr |> parzerker.e_map(fn(expr) { ast.ParenExpression(expr) }) },
          e_rparen,
        )
      },
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
      "(",
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
      ")",
    )
    |> parzerker.e_map(fn(_) { token.Rightparen })
  }
}

pub fn e_expr(state) {
  state
  |> {
    e_suffix
    |> parzerker.e_or(e_prefix, list.append)
    |> parzerker.e_or(
      { e_literal |> parzerker.e_map(fn(lit) { ast.Literal(lit) }) },
      list.append,
    )
  }
}

pub fn e_stmt(state) {
  e_expr(state)
}

pub fn e_decl(state) {
  e_stmt(state)
}

pub fn e_plus(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Plus -> True
          _ -> False
        }
      },
      False,
      "+",
    )
    |> parzerker.e_map(fn(_) { ast.Plus })
  }
}

pub fn e_minus(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Minus -> True
          _ -> False
        }
      },
      False,
      "-",
    )
    |> parzerker.e_map(fn(_) { ast.Minus })
  }
}

pub fn e_and(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.And -> True
          _ -> False
        }
      },
      False,
      "&",
    )
    |> parzerker.e_map(fn(_) { ast.And })
  }
}

pub fn e_bang(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Bang -> True
          _ -> False
        }
      },
      False,
      "!",
    )
    |> parzerker.e_map(fn(_) { ast.Bang })
  }
}

pub fn e_tilde(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Tilde -> True
          _ -> False
        }
      },
      False,
      "~",
    )
    |> parzerker.e_map(fn(_) { ast.Tilde })
  }
}

pub fn e_op_or(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Or -> True
          _ -> False
        }
      },
      False,
      "|",
    )
    |> parzerker.e_map(fn(_) { ast.Or })
  }
}

pub fn e_operator(state) {
  state
  |> {
    e_plus
    |> parzerker.e_or(e_minus, list.append)
    |> parzerker.e_or(e_and, list.append)
    |> parzerker.e_or(e_op_or, list.append)
  }
}

pub fn e_unary(state) {
  state
  |> {
    e_minus
    |> parzerker.e_or(e_bang, list.append)
    |> parzerker.e_or(e_tilde, list.append)
  }
}

pub fn e_suffix(state) {
  state
  |> {
    e_literal
    |> parzerker.e_seq(e_unary, fn(lit, op) { ast.Suffix(lit, op) })
  }
}

pub fn e_prefix(state) {
  state
  |> {
    e_unary
    |> parzerker.e_seq(e_literal, fn(op, lit) { ast.Prefix(op, lit) })
  }
}

pub fn e_file(state) {
  state
  |> { e_decl |> parzerker.e_cont0 }
}
