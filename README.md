# RedHDL

RedHDL is a hardware description language that compiles to Minecraft Redstone circuits as schematic files. Currently, only [Sponge specification](https://github.com/SpongePowered/Schematic-Specification) (which is the default format used by [WorldEdit](https://github.com/EngineHub/WorldEdit)) is supported.

## Installation

You can use the [released artifacts](https://github.com/Iltotore/redhdl/releases) (JAR or native executable).

You can also run the project from source using:

```
./millw cli.run <args>
```

Note: requires JDK 17+.

For Windows, use `millw.bat` instead of `millw`

## Usage

```
redhdl [--output <path>] [--entrypoint <string>] [--no-optimize] [--no-align] [--palette <string>]... [--repeater-delay <integer>] <path>
```

Options and flags:
- `--help` Display this help text.
- `--version` Print the version number and exit.
- `--output <path>`, `-o <path>`: Path to write the schematic to
- `--entrypoint <string>`, `-e <string>`: Program entrypoint
- `--no-optimize`: Disable optimizations
- `--no-align`: Disable output nodes alignment
- `--palette <string>`: Block id to use for wires or alias (rainbow)
- `--repeater-delay <integer>`: Set the repeater delay between 1 (fastest) and 4 (longest)

Example:

```
redhdl my_component.red -o schematic/my_component.schem
```

Note that the compiled file must contain a component named either `Main`, the same name of the input file ignoring case or the name specified using `--entrypoint`/`-e`.

## Building the project

The project can be compiled using the following command:

```
./millw <module>.compile
```

where `<module>` is either `main`, `main.test` or `cli`. Since it depends on the former, compiling `cli` also compiles `main`.

The tests can be run using:

```
./millw main.test
```

Finally, you can build the JAR (Java bytecode) artifact (stored in `out/cli/assembly.dest/`) using 

```
./millw cli.assembly
```

and for the native executable (stored in `out/cli/nativeLink.dest/`):

```
./millw cli.nativeLink
```

## Internals

### Ecosystem

We used the [Scala programming language](https://scala-lang.org/) for building RedHDL. Some of its features relevant such as FP capabilities including ADTs and its
vast ecosystem of parsing libraries make it relevant for compiler development. The build tool used for this project is [Mill](https://mill-build.com/).

We built RedHDL upon [Kyo](https://getkyo.io/), an ecosystem based on effect handlers providing the guarantees of monads while vastly improving composability.

Used libraries:

- [Kyo](https://getkyo.io/): effect handlers
- Kyo Direct: alternative "coroutine-like" syntax for Kyo
- Kyo Parse: parser combinators, used for the lexer and syntactic parser
- [Iron](https://github.com/Iltotore/iron/): refined types to strenghten the typing system and prevent more bugs at compile-time
- [Decline](https://ben.kirw.in/decline/) (CLI module only): composable command-line parsing

Note: we originally used two libraries for respectively schematic generation and NBT serialization but we ended up writing our own implementation for both
code readability purpose and unlocking Scala Native compilation.

### Compilation

The compiler contains multiple phases from textual source code to schematic output:

- Lexing: turn the source code (`String`) to a list of tokens
- Parsing: build an Abstract Syntax Tree (AST) from the tokens
- Typechecking: ensure the used types, subcomponents and ports exist
- Component expansion: expand the subcomponents in order to have a single uber component
- Component simplification: simplify expressions such as double boolean negation or boolean operators with literal operands
- Graph building: build a graph where nodes are primitive operators and edges are links between ports
- Graph layering: the graph is divided into layers (using topological sort) and nets. "Relay" gates are added if two linked gates are not on adjacent layers.
- Net routing: each net is routed from input to output port using left-edge algorithm
- Structure generation: the routed graph is compiled to a 3D block array
- Structure saving: the structure is serialized to GZIP-ed NBT following the [Sponge specification](https://github.com/SpongePowered/Schematic-Specification)

