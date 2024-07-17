import argv
import gleam/erlang.{get_line as input}
import gleam/io
import gleam/string
import lexing
import lib
import parsing
import parzerker
import simplifile
import token

pub fn main() {
  lexing.lexing_test()
  parsing.parsing_test()
  startup()
}

pub fn startup() {
  case argv.load().arguments {
    [] -> {
      io.print(
        "Welcome to the bang! interactive environment.\n"
        <> "tokenizes and parses input.\n"
        <> "enter · to exit.\n\n",
      )
      repl()
    }
    [path] ->
      case simplifile.read(path) {
        Ok(source) -> interpret(source)
        Error(e) ->
          {
            "Could not read file: "
            <> path
            <> "\n"
            <> simplifile.describe_error(e)
          }
          |> io.println()
      }
    _ -> io.println("Usage:\n - './bang' -> repl\n - './bang <path>' -> file")
  }
}

pub fn repl() {
  case input("!> ") {
    Ok(str) -> {
      let assert Ok(#(grapheme, _)) = string.pop_grapheme(str)
      case grapheme {
        "·" -> io.println("Goodbye!")
        _ -> {
          interpret2(str)
          repl()
        }
      }
    }
    Error(_) -> {
      io.println("Couldn't get line.")
      repl()
    }
  }
}

fn interpret(str: String) {
  case str |> lexing.init_state_str() |> lexing.lex() {
    parzerker.ESuccess(_, tokens) -> {
      io.println("Successfully tokenized input:")
      tokens |> token.repr_tokens() |> io.println()

      case
        tokens
        |> token.drop_ws()
        |> parsing.init_state_token()
        |> parsing.e_decl()
      {
        parzerker.ESuccess(_, decl) -> {
          io.println("Successfully parsed tokens:")
          decl |> io.debug()
          Nil
        }
        parzerker.EFailure(_, errors, fatal) ->
          lib.error_str(errors, fatal) |> io.println()
      }
    }
    parzerker.EFailure(_, errors, fatal) ->
      lib.error_str(errors, fatal) |> io.println()
  }
}

fn interpret2(str) {
  let out = {
    let state = str |> lexing.init_state_str()

    use _, lexed <- on_success(lexing.lex(state))

    io.println("Successfully tokenized input:")
    lexed |> token.repr_tokens() |> io.println()

    let state = lexed |> parsing.init_state_token()

    use new, parsed <- on_success(parsing.e_file(state))

    io.println("Successfully parsed tokens:")
    io.debug(parsed)

    parzerker.ESuccess(new, parsed)
  }
  case { out } {
    parzerker.ESuccess(_, _) -> Nil
    parzerker.EFailure(_, errors, fatal) ->
      lib.error_str(errors, fatal) |> io.println()
  }
}

fn on_success(res, func) {
  case res {
    parzerker.ESuccess(new, parsed) -> func(new, parsed)
    parzerker.EFailure(tally, error, fatal) ->
      parzerker.EFailure(tally, error, fatal)
  }
}
