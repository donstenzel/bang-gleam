import ast
import gleam/io
import gleam/list
import token

import parzerker

pub fn parsing_test() {
  [
    token.Leftparen,
    token.Identifier("test"),
    token.Colon,
    token.Identifier("String"),
    token.Rightparen,
    token.Bind,
    token.Number(10),
  ]
  |> { init_state_token }
  |> { e_anon_func }
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

pub fn e_literal(
  state,
) -> parzerker.EResult(token.Token, ast.Literal, List(String)) {
  state
  |> {
    e_number
    |> parzerker.e_or(e_string, list.append)
    |> parzerker.e_or(e_bool, list.append)
    |> parzerker.e_or(e_ref, list.append)
    |> parzerker.e_or(
      {
        { e_expr |> parzerker.e_map(fn(expr) { ast.ParenExpression(expr) }) }
        |> parzerker.e_surr(e_lparen, e_rparen)
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

pub fn e_lit_expr(state) {
  state
  |> { e_literal |> parzerker.e_map(ast.Literal) }
}

pub fn e_expr(state) {
  state
  |> {
    // e_suffix
    // |> parzerker.e_or(e_prefix, list.append)
    // |> parzerker.e_or(e_lit_expr, list.append)
    e_lit_expr
    |> parzerker.e_or(e_callchain, list.append)
    |> parzerker.e_or(precedence_6, list.append)
    |> parzerker.e_or(e_anon_func, list.append)
  }
}

pub fn e_stmt(state) {
  state
  |> {
    e_expr
    |> parzerker.e_map(fn(expr) { ast.Expression(expr) })
  }
}

pub fn e_decl(state) {
  e_stmt(state)
}

pub fn e_mult(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Star -> True
          _ -> False
        }
      },
      False,
      "*",
    )
    |> parzerker.e_map(fn(_) { ast.Multiply })
  }
}

pub fn e_divide(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Slash -> True
          _ -> False
        }
      },
      False,
      "/",
    )
    |> parzerker.e_map(fn(_) { ast.Divide })
  }
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

pub fn e_lshift(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Leftshift -> True
          _ -> False
        }
      },
      False,
      "<<",
    )
    |> parzerker.e_map(fn(_) { ast.LeftShift })
  }
}

pub fn e_rshift(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Rightshift -> True
          _ -> False
        }
      },
      False,
      ">>",
    )
    |> parzerker.e_map(fn(_) { ast.RightShift })
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

pub fn e_colon(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Colon -> True
          _ -> False
        }
      },
      False,
      ":",
    )
    |> parzerker.e_map(fn(_) { token.Colon })
  }
}

pub fn e_bind(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Bind -> True
          _ -> False
        }
      },
      False,
      "->",
    )
    |> parzerker.e_map(fn(_) { token.Bind })
  }
}

pub fn e_gt(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Greater -> True
          _ -> False
        }
      },
      False,
      ">",
    )
    |> parzerker.e_map(fn(_) { ast.Greater })
  }
}

pub fn e_ge(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Greaterequals -> True
          _ -> False
        }
      },
      False,
      ">=",
    )
    |> parzerker.e_map(fn(_) { ast.GreaterEquals })
  }
}

pub fn e_lt(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Less -> True
          _ -> False
        }
      },
      False,
      "<",
    )
    |> parzerker.e_map(fn(_) { ast.Less })
  }
}

pub fn e_le(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Lessequals -> True
          _ -> False
        }
      },
      False,
      "<=",
    )
    |> parzerker.e_map(fn(_) { ast.LessEquals })
  }
}

pub fn e_eq(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Dequals -> True
          _ -> False
        }
      },
      False,
      "==",
    )
    |> parzerker.e_map(fn(_) { ast.IsEqual })
  }
}

pub fn e_neq(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Notequals -> True
          _ -> False
        }
      },
      False,
      "!=",
    )
    |> parzerker.e_map(fn(_) { ast.IsNotEqual })
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
    precedence_0
    |> parzerker.e_seq(e_unary, fn(lit, op) { ast.Suffix(lit, op) })
  }
}

pub fn e_prefix(state) {
  state
  |> {
    e_unary
    |> parzerker.e_seq(precedence_0, fn(op, lit) { ast.Prefix(op, lit) })
  }
}

pub fn e_comma(state) {
  state
  |> {
    parzerker.e_if(
      fn(t) {
        case t {
          token.Comma -> True
          _ -> False
        }
      },
      False,
      ",",
    )
    |> parzerker.e_map(fn(_) { token.Comma })
  }
}

pub fn e_comma_sep_exprs(state) {
  state
  |> { e_expr |> parzerker.e_sep_by0(e_comma) |> parzerker.e_opt([]) }
}

fn calls(prev, rest) {
  case rest {
    [] -> prev
    [head, ..tail] -> calls(ast.Call(prev, head), tail)
  }
}

pub fn e_callchain(state) {
  state
  |> {
    e_literal
    |> parzerker.e_seq(
      { e_comma_sep_exprs |> parzerker.e_surr(e_lparen, e_rparen) }
        |> parzerker.e_cont1,
      fn(lit, exprlists) { calls(ast.Literal(lit), exprlists) },
    )
  }
}

// could abstract precedence levels
pub fn precedence_0(state) {
  state
  |> {
    e_callchain
    |> parzerker.e_or(e_lit_expr, list.append)
  }
}

pub fn precedence_1(state) {
  state
  |> {
    //{ e_suffix |> parzerker.e_or(e_prefix, list.append) }
    e_prefix
    |> parzerker.e_or(precedence_0, list.append)
  }
}

pub fn binary_chain(seperator, element, operator) {
  element
  |> parzerker.e_sep_by1(seperator)
  |> parzerker.e_map(fn(elems) {
    let assert [head1, ..tail1] = elems
    let assert [head2, ..tail2] = tail1

    ast.Binary(head1, operator, head2)
    |> list.fold(over: tail2, with: fn(prev, curr) {
      ast.Binary(prev, operator, curr)
    })
  })
}

pub fn precedence_2(state) {
  state
  |> {
    binary_chain(e_mult, precedence_1, ast.Multiply)
    |> parzerker.e_or(
      binary_chain(e_divide, precedence_1, ast.Divide),
      list.append,
    )
    |> parzerker.e_or(precedence_1, list.append)
  }
}

pub fn precedence_3(state) {
  state
  |> {
    binary_chain(e_plus, precedence_2, ast.Plus)
    |> parzerker.e_or(
      binary_chain(e_minus, precedence_2, ast.Minus),
      list.append,
    )
    |> parzerker.e_or(precedence_2, list.append)
  }
}

pub fn precedence_4(state) {
  state
  |> {
    binary_chain(e_lshift, precedence_3, ast.LeftShift)
    |> parzerker.e_or(
      binary_chain(e_rshift, precedence_3, ast.RightShift),
      list.append,
    )
    |> parzerker.e_or(precedence_3, list.append)
  }
}

pub fn precedence_5(state) {
  state
  |> {
    binary_chain(e_gt, precedence_4, ast.Greater)
    |> parzerker.e_or(
      binary_chain(e_ge, precedence_4, ast.GreaterEquals),
      list.append,
    )
    |> parzerker.e_or(binary_chain(e_lt, precedence_4, ast.Less), list.append)
    |> parzerker.e_or(
      binary_chain(e_le, precedence_4, ast.LessEquals),
      list.append,
    )
    |> parzerker.e_or(precedence_4, list.append)
  }
}

pub fn precedence_6(state) {
  state
  |> {
    binary_chain(e_eq, precedence_5, ast.IsEqual)
    |> parzerker.e_or(
      binary_chain(e_neq, precedence_5, ast.IsNotEqual),
      list.append,
    )
    |> parzerker.e_or(precedence_5, list.append)
  }
}

pub fn e_param(state) {
  state
  |> {
    e_ident
    |> parzerker.e_seq({ e_colon |> parzerker.e_seq_r(e_ref) }, fn(l, r) {
      let assert token.Identifier(name) = l
      let assert ast.Reference(ref) = r
      #(name, ref)
    })
  }
}

pub fn e_params(state) {
  state
  |> {
    e_param
    |> parzerker.e_sep_by0(e_comma)
    |> parzerker.e_opt([])
    |> parzerker.e_surr(e_lparen, e_rparen)
  }
}

pub fn e_anon_func(state) {
  state
  |> {
    e_params
    |> parzerker.e_seq_l(e_bind)
    |> parzerker.e_seq(e_stmt, fn(l, r) { ast.AnonFunction(l, r) })
  }
}

pub fn e_file(state) {
  state
  |> { e_decl |> parzerker.e_cont1 }
}
