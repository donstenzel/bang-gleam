import argv
import gleam/erlang.{get_line as input}
import gleam/io
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
        "Welcome to the bang! interactive environment. for now only tokenizes your input\n\n",
      )
      repl(lexing.lex)
    }
    [path] ->
      case simplifile.read(path) {
        Ok(source) ->
          case source |> lexing.init_state_str |> lexing.lex() {
            parzerker.ESuccess(_, tokens) ->
              tokens |> token.repr_tokens() |> io.println()
            parzerker.EFailure(_, errors, fatal) ->
              lib.error_str(errors, fatal) |> io.println()
          }
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

pub fn repl(parse: parzerker.EFunction(String, List(token.Token), List(String))) {
  case input("|> ") {
    Ok(str) ->
      case str |> lexing.init_state_str() |> parse() {
        parzerker.ESuccess(_, tokens) ->
          tokens |> token.repr_tokens() |> io.println()
        parzerker.EFailure(_, errors, fatal) ->
          lib.error_str(errors, fatal) |> io.println()
      }
    Error(_) -> io.println("Couldn't get line.")
  }
  repl(parse)
}
