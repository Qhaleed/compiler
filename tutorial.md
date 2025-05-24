Res Programming Language Syntax

## Overview

Res is a simple programming language designed for basic computations and output. It supports variable declarations, printing, functions, function calls, and comments. The syntax is inspired by JavaScript's arrow functions for function definitions and uses a unique keyword style (e.g., "typeshi" for declarations, "coms" for printing).

## Syntax Elements

1. **Variable Declarations**

   - Syntax: `typeshi <type> <identifier> = <expression>;`
   - Types: `int` (integer), `float` (floating-point), `string` (text)
   - Initializes a variable with an expression.
   - Example:
     ```
     typeshi int x = (2 + 3) * 4;
     typeshi string s = "hello";
     typeshi float f = 3.14;
     ```

2. **Print Statements**

   - Syntax: `coms(<expression>, <expression>, ...);`
   - Prints expressions to the console, separated by spaces, followed by a newline.
   - Example:
     ```
     coms(x, "test", 42); // Outputs: <value_of_x> test 42
     ```

3. **Function Definitions**

   - Syntax: `func <identifier>(<type> <identifier>, ...) => { <statement>* }`
   - Defines a function with parameters and a body of statements.
   - Functions are assumed to return void or the type of their first parameter (no explicit return statements).
   - Example:
     ```
     func add(int a, int b) => {
         typeshi int sum = a + b;
         coms(sum);
     }
     ```

4. **Function Calls**

   - Syntax: `<identifier>(<expression>, ...);`
   - Calls a previously defined function with arguments.
   - Example:
     ```
     add(5, 3); // Calls add, prints: 8
     ```

5. **Expressions**

   - Supported: Numbers, string literals, variables, binary operations, and parenthesized expressions.
   - Operators:
     - Arithmetic: `+`, `-`, `*`, `/`
     - Comparison: `==`, `<`, `>`, `<=`, `>=`
     - Logical: `&&`, `||`
     - Assignment: `=` (used in declarations)
   - Examples:
     ```
     (2 + 3) * 4
     x + 1
     "hello" == "world"
     ```

6. **Comments**
   - Syntax: `// <text>`
   - Single-line comments ignored by the compiler.
   - Example:
     ```
     // This is a comment
     ```

## Notes

- **Input Handling**: Programs are entered interactively in a console, with compilation triggered by `\g` on a new line.
- **Output**: The Res compiler generates C++ code, which is printed to the console and saved to a file (e.g., `program.cpp`).
- **Limitations**:
  - No explicit return statements in functions (extendable if needed).
  - Simple expression parsing (operators wrapped in parentheses, no strict precedence).
  - Global variable scope (local scopes can be added).

## Example Program

```
typeshi int number = 32;
coms(number, "test", 42);
// This is a comment
func add(int a, int b) => {
    typeshi int sum = a + b;
    coms(sum);
}
add(5, 3);
```

This program declares a variable, prints values, defines a function, and calls it, demonstrating all major features of Res.
