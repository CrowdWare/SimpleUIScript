# Simple UI Script

A small, extendable interpreter for a minimalist scripting language implemented in Kotlin using the [better-parse](https://github.com/h0tk3y/better-parse) library.

## Features

- **Variables**: `var name = expression`
- **Data Types**: Integer, String, Boolean
- **Expressions**:
  - **Arithmetic**: `+`, `-`, `*`, `/`
  - **Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=`
  - **Logical**: `&&`, `||`, unary `!`, unary `+`, unary `-`
- **Control Flow**:
  - `if (condition) { ... } else { ... }`
  - `while (condition) { ... }`
- **Function Calls**: built-in `submit()` and `showAlert(message)` for side effects
- **AST-based**: clear separation between parsing and evaluation

## Getting Started

### Prerequisites

- JDK 8+ or later
- Gradle or Maven

### Build

Using Gradle:

```bash
./gradlew build
```

Using Maven:

```bash
mvn clean package
```

### Run Examples

You can run it from your IDE or via a main launcher, for example:

```bash
# If you have an application plugin set up in Gradle:
./gradlew run
```

## Extending the Interpreter

This minimal interpreter can be extended with:

- **Loops**: `for`, `do-while`
- **Functions**: user-defined functions, parameters, `return`
- **Scope Management**: local vs. global variables
- **Data Structures**: Lists, Maps, Arrays
- **Error Handling**: richer error messages, exception handling (`throw`/`catch`)
- **Standard Library**: `print()`, `toString()`, math utilities

To add new grammar rules, update `Parser.kt` and extend `Interpreter.evaluateExpression` or `executeStatement` accordingly.

## Contributing

Feel free to open issues or submit pull requests to add features or fix bugs. Please follow the existing code style and add tests for new functionality.

## License

This project is released under the GPL3 License.

