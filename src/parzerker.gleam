import gleam/int
import gleam/list
import gleam/order
import gleam/string
import lib

pub type State(i) {
  EState(rest: List(i), tally: Int)
}

pub type EResult(i, s, e) {
  ESuccess(new: State(i), eaten: s)
  EFailure(tally: Int, error: e, fatal: Bool)
}

pub fn is_success(r: EResult(_, _, _)) -> Bool {
  case r {
    ESuccess(_, _) -> True
    EFailure(_, _, _) -> False
  }
}

pub fn is_error(r: EResult(_, _, _)) -> Bool {
  case r {
    ESuccess(_, _) -> False
    EFailure(_, _, _) -> True
  }
}

pub fn is_tally(r: EResult(_, _, _), to_match) -> Bool {
  case r {
    ESuccess(EState(_, tally), _) -> tally == to_match
    EFailure(tally, _, _) -> tally == to_match
  }
}

pub type EFunction(input, parsed, error) =
  fn(State(input)) -> EResult(input, parsed, error)

pub fn e_seq(
  e1: EFunction(i, s1, e),
  e2: EFunction(i, s2, e),
  combine_eaten: fn(s1, s2) -> s_new,
) -> EFunction(i, s_new, e) {
  fn(state: State(i)) -> EResult(i, s_new, e) {
    case e1(state) {
      EFailure(state, error, fatal) -> EFailure(state, error, fatal)
      ESuccess(new, eaten1) ->
        case e2(new) {
          EFailure(state, error, fatal) -> EFailure(state, error, fatal)
          ESuccess(new, eaten2) -> ESuccess(new, combine_eaten(eaten1, eaten2))
        }
    }
  }
}

pub fn e_seq_l(
  e1: EFunction(i, s, e),
  e2: EFunction(i, _, e),
) -> EFunction(i, s, e) {
  e_seq(e1, e2, fn(l, _) { l })
}

pub fn e_seq_r(
  e1: EFunction(i, _, e),
  e2: EFunction(i, s, e),
) -> EFunction(i, s, e) {
  e_seq(e1, e2, fn(_, r) { r })
}

pub fn e_surr(
  l: EFunction(i, _, e),
  e: EFunction(i, s, e),
  r: EFunction(i, _, e),
) -> EFunction(i, s, e) {
  e_seq_r(l, e_seq_l(e, r))
}

pub fn e_seq_many(
  es: List(EFunction(i, s, e)),
  combine_eaten: fn(s, s) -> s,
) -> EFunction(i, s, e) {
  case es {
    [head, ..tail] -> {
      list.fold(from: head, over: tail, with: fn(e1, e2) {
        e_seq(e1, e2, combine_eaten)
      })
    }
    [] -> panic as "Cannot sequence 0 EFunctions."
  }
}

pub fn e_or(
  e1: EFunction(i, s, e),
  e2: EFunction(i, s, e),
  combine_errors: fn(e, e) -> e,
) -> EFunction(i, s, e) {
  fn(state: State(i)) -> EResult(i, s, e) {
    case e1(state), e2(state) {
      ESuccess(EState(_, tally1), _) as s1, ESuccess(EState(_, tally2), _) as s2
      -> {
        case int.compare(tally1, tally2) {
          order.Lt -> s2
          order.Eq
          | // <- maybe return error because of ambiguous match
            order.Gt -> s1
        }
      }

      EFailure(_, _, _), ESuccess(_, _) as s
      | ESuccess(_, _) as s, EFailure(_, _, _)
      -> s

      EFailure(tally1, error1, fatal1) as f1,
        EFailure(tally2, error2, fatal2) as f2
      -> {
        case int.compare(tally1, tally2) {
          order.Lt -> f2
          order.Gt -> f1
          order.Eq ->
            EFailure(tally1, combine_errors(error1, error2), fatal1 || fatal2)
        }
      }
    }
  }
}

pub fn e_opt(
  e: EFunction(i, s, e),
  zero: s,
  // the zero element under this operation over s.
) -> EFunction(i, s, e) {
  fn(state: State(i)) -> EResult(i, s, e) {
    case e(state) {
      EFailure(_, _, _) -> ESuccess(state, zero)
      ESuccess(_, _) as s -> s
    }
  }
}

pub fn e_cont0(e: EFunction(i, s, e)) -> EFunction(i, List(s), e) {
  e_cont0_rec(e, _)
}

pub fn e_cont0_rec(
  e: EFunction(i, s, e),
  state: State(i),
) -> EResult(i, List(s), e) {
  case e(state) {
    ESuccess(new, eaten) ->
      case e_cont1_rec(e, new) {
        ESuccess(newer, eaten_tail) -> ESuccess(newer, [eaten, ..eaten_tail])
        EFailure(_, _, _) -> ESuccess(new, [eaten])
      }
    EFailure(_, _, _) -> ESuccess(state, [])
  }
}

pub fn e_cont1(e: EFunction(i, s, e)) -> EFunction(i, List(s), e) {
  e_cont1_rec(e, _)
}

pub fn e_cont1_rec(
  e: EFunction(i, s, e),
  state: State(i),
) -> EResult(i, List(s), e) {
  case e(state) {
    ESuccess(new, eaten) ->
      case e_cont1_rec(e, new) {
        ESuccess(newer, eaten_tail) -> ESuccess(newer, [eaten, ..eaten_tail])
        EFailure(_, _, _) -> ESuccess(new, [eaten])
      }
    EFailure(tally, errors, fatal) -> EFailure(tally, errors, fatal)
  }
}

pub fn e_map(e: EFunction(i, s, e), f: fn(s) -> s_new) -> EFunction(i, s_new, e) {
  fn(state: State(i)) -> EResult(i, s_new, e) {
    case e(state) {
      EFailure(state, error, fatal) -> EFailure(state, error, fatal)
      ESuccess(new, eaten) -> ESuccess(new, f(eaten))
    }
  }
}

pub fn e_map_if(
  e: EFunction(i, s, List(String)),
  p: fn(s) -> Bool,
  description: String,
) -> EFunction(i, s, List(String)) {
  fn(state: State(i)) -> EResult(i, s, List(String)) {
    case e(state) {
      EFailure(_, _, _) as f -> f
      ESuccess(_, eaten) as s ->
        case p(eaten) {
          True -> s
          False -> EFailure(state.tally, [description], False)
        }
    }
  }
}

pub fn e_map_err(
  e: EFunction(i, s, e),
  f: fn(e) -> e_new,
) -> EFunction(i, s, e_new) {
  fn(state: State(i)) -> EResult(i, s, e_new) {
    case e(state) {
      ESuccess(new, eaten) -> ESuccess(new, eaten)
      EFailure(tally, error, fatal) -> EFailure(tally, f(error), fatal)
    }
  }
}

pub fn e_until(
  e: EFunction(i, s, e),
  end: EFunction(i, s, e),
) -> EFunction(i, List(s), e) {
  e_until_inner(_, e, end)
}

pub fn e_until_inner(
  state: State(i),
  e: EFunction(i, s, e),
  end: EFunction(i, s, e),
) -> EResult(i, List(s), e) {
  case end(state) {
    EFailure(_, _, _) ->
      case e(state) {
        ESuccess(new, eaten) ->
          case e_until_inner(new, e, end) {
            ESuccess(newer, eaten_tail) -> {
              ESuccess(newer, [eaten, ..eaten_tail])
            }
            EFailure(tally, errors, fatal) -> EFailure(tally, errors, fatal)
          }
        EFailure(tally, errors, fatal) -> EFailure(tally, errors, fatal)
      }
    ESuccess(newer, eaten2) -> ESuccess(newer, [eaten2])
  }
}

pub fn e_if(
  predicate: fn(i) -> Bool,
  fatal: Bool,
  description: String,
  // describes the predicate
) -> EFunction(i, i, List(String)) {
  fn(state: State(i)) -> EResult(i, i, List(String)) {
    let EState(rest, tally) = state

    case rest {
      [head, ..tail] ->
        case predicate(head) {
          True -> ESuccess(EState(tail, tally + 1), head)
          False ->
            EFailure(
              tally,
              [
                "Input @"
                <> int.to_string(tally)
                <> " >  "
                <> { head |> string.inspect() |> lib.escape() }
                <> " did not match: "
                <> description,
              ],
              fatal,
            )
        }
      [] -> EFailure(tally, ["No input left to match."], True)
    }
  }
}
