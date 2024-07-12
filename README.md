# ⚡ bang!

### In your terminal:
```sh
# add bang to your project with:
gleam add bang
```

### In gleam:
```gleam
// then import it
import bang

pub fn main() {
  // finally, use the parse function:
  //    1. string -> [token]
  //    2. [token] -> AST
  //    3. AST -> Output / Bytecode? Binary? Transpilation?
  let tokens = parse("fun add(a, b) { return a + b }")
  // -> [Function,
  //     Ident("add"),
  //     Leftparen,
  //     Ident("a"),
  //     Comma,
  //     Whitespace("\\s"),
  //     Ident("b"),
  //     Rightparen,
  //     Whitespace("\\s"),
  //     Leftbrace,
  //     Whitespace("\\s"),
  //     Return,
  //     Ident("a"),
  //     Whitespace("\\s"),
  //     Plus,
  //     Whitespace("\\s"),
  //     Ident("b"),
  //     Whitespace("\\s"),
  //     Rightbrace]
}
```

Further documentation can be found at <https://hexdocs.pm/bang>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
