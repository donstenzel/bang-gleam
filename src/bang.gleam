import argv
import color
import gleam/erlang.{get_line as input}
import gleam/io
import gleam/string
import lexing
import lib
import parsing
import parzerker
import simplifile

pub fn main() {
  // lexing.lexing_test()
  // "(a) -> 10" |> lexing.init_state_str() |> lexing.lex() |> io.debug()
  // parsing.parsing_test()

  color.seperated([
    #(#(10, 40, 100), "Lex"),
    #(#(150, 10, 60), "Parse"),
    #(#(40, 150, 20), "Analyze"),
    #(#(120, 40, 190), "Evaluate"),
  ])
  |> io.println()
  // startup()
}

fn color_bang() {
  color.get_rgb_code_clamped(98, 22, 234)
}

pub fn startup() {
  case argv.load().arguments {
    [] -> {
      io.print(
        "Welcome to the "
        <> "bang!" |> color.colored(color_bang())
        <> " interactive environment.\n"
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
          |> color.colored(color.error())
          |> io.println()
      }
    _ ->
      {
        "Invalid invocation."
        <> color.hint()
        <> "Usage:\n - './bang' -> repl\n - './bang <path>' -> file"
      }
      |> color.colored(color.error())
      |> io.println()
  }
}

pub fn repl() {
  case input(color_bang() <> "·!> " <> color.off) {
    Ok(str) -> {
      let assert Ok(#(grapheme, _)) = string.pop_grapheme(str)
      case grapheme {
        "·" -> io.println(color.success() <> "Goodbye!" <> color.off)
        _ -> {
          interpret(str)
          repl()
        }
      }
    }
    Error(_) -> {
      io.println(color.error() <> "Couldn't get line." <> color.off)
      repl()
    }
  }
}

fn interpret(str) {
  let out = {
    let state = str |> lexing.init_state_str()

    use _, lexed <- on_success(lexing.lex(state))
    io.println(color.success() <> "Successfully tokenized input." <> color.off)
    let state =
      lexed |> lexing.drop_ws() |> parsing.init_state_token() |> io.debug()

    use new, parsed <- on_success(parsing.e_file(state))

    io.println(color.success() <> "Successfully parsed tokens:")
    io.debug(parsed)
    io.print(color.off)

    parzerker.ESuccess(new, parsed)
  }
  case out {
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
