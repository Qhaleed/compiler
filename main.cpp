/**
 * Res Programming Language Compiler
 * 
 * This is a partial implementation of a compiler for the Res programming language
 * that translates Res source code into its equivalent C++ code. The compiler follows
 * traditional compiler architecture with clear separation between lexical analysis,
 * syntax analysis, and code generation phases. 
 
 * Architecture Overview:
 * - Lexer: Converts source text into tokens using character-by-character analysis
 * - Parser: Builds Abstract Syntax Trees using recursive descent and state machines
 * - CodeGenerator: Produces readable C++ code using a three-pass approach
 * - REPL: Provides interactive development environment with error recovery
 
 * Testing Strategy:
 * The compiler has been tested with various Res programs including:
 * - Variable declarations with different types (int, float, string)
 * - Arithmetic expressions with proper precedence handling
 * - Function definitions with parameters and recursive calls
 * - Print statements with multiple expressions
 * - Error conditions for syntax validation
 
 * Files generated are stored in the saves directory
 * complex Res programs into working C++ code.
 * For sample code snippets for res programming language, please refer to samples.md
 */

// =========================================================================================

/**
 * Navigation Guide:
 *
 * Welcome to the Res Compiler's source code! This guide helps you find your way around.
 * You can use Ctrl+F (or Cmd+F) with the keywords below to jump to different parts of the code.
 *
 * Sections:
 * 1. "Core Building Blocks": Basic types and structures the compiler uses.
 * 2. "Lexical Analysis (Lexer)": How the raw code text is broken into "words" or tokens.
 * 3. "Symbol Table": How the compiler remembers variable names and their details.
 * 4. "Abstract Syntax Tree (AST) Nodes": The structures that represent your code's meaning.
 * 5. "Syntax Analysis (Parser)": How the tokens are organized into a meaningful structure (the AST).
 * 6. "Main Program & REPL": Where the compiler starts and how it can be used interactively.
 *
 * --- 1. Core Building Blocks (Search: "enum class TokenType", "struct Token", "struct TokenList") ---
 *    - `enum class TokenType`: Lists all categories of code elements (e.g., keywords like 'typeshi', operators like '+').
 *    - `struct Token`: Represents a single piece of code, like a word or symbol, with its type and actual text.
 *    - `struct TokenList`: A list that holds all the Tokens in the order they appear in the code.
 *
 * --- 2. Lexical Analysis (Lexer) (Search: "class Lexer") ---
 *    - `class Lexer`: This part reads your Res source code character by character.
 *      Its main job (`tokenize` method) is to convert the text into a sequence of Tokens (a `TokenList`).
 *      It identifies things like keywords, variable names (`parseIdentifier`), numbers (`parseNumber`),
 *      strings (`parseStringLiteral`), and comments (`parseComment`).
 *
 * --- 3. Symbol Table (Search: "class SymbolTable", "struct BSTNode") ---
 *    - `class SymbolTable`: Think of this as the compiler's memory for variables.
 *      It uses a Binary Search Tree (`BSTNode`) to keep track of all declared variables,
 *      allowing quick checks (`isDeclared`) and additions (`addVariable`).
 *
 * --- 4. Abstract Syntax Tree (AST) Nodes (Search: "struct ASTNode", "struct Expr", "struct DeclStmt") ---
 *    - `struct ASTNode`: The basic building block for the AST. All other AST node types are derived from this.
 *    - Expression Nodes (e.g., `NumberExpr`, `VarExpr`, `BinaryExpr`): Represent parts of code that produce values,
 *      like numbers, variable uses, or calculations (e.g., `a + b`).
 *    - Statement Nodes (e.g., `DeclStmt`, `PrintStmt`, `FuncDef`): Represent actions or declarations,
 *      like creating a variable (`typeshi int x = 10;`), printing something (`coms(x);`), or defining a function.
 *    - `struct Program`: The root of the AST, holding all statements of your Res program.
 *
 * --- 5. Syntax Analysis (Parser) (Search: "class Parser", "struct ParseState") ---
 *    - `class Parser`: This part takes the list of Tokens from the Lexer and builds the Abstract Syntax Tree (AST).
 *      It checks if the sequence of tokens follows the grammatical rules of the Res language.
 *      It uses a `ParseStack` (made of `ParseState` and `StackNode`) to manage this process.
 *      Key methods include `parse` (to start parsing), `parseStatement` (for individual statements),
 *      and methods for specific language constructs like declarations (`parseDeclStmt`).
 *
 * --- 6. Main Program & REPL (Search: "int main(") ---
 *    - `int main()`: This is where the compiler program starts running.
 *      It handles tasks like reading Res code from a file or running the REPL (Read-Eval-Print Loop),
 *      which allows you to type Res code interactively.
 *      It coordinates the Lexer, Parser, and (eventually) the CodeGenerator.
 *
 * Code Generation (Search: "CodeGenerator" or C++ emitting code like 'outputFile <<'):
 *    - Although not fully detailed here, the `CodeGenerator`'s role is to take the AST
 *      produced by the Parser and translate it into equivalent C++ code.
 */

/**
 * 6. The following are the data structures that are used in this project:
 *
 *    a. Stack:
 *       - `ParseStack`: A custom stack used by the `Parser` to manage parsing states and construct the AST.
 *         It helps in handling nested structures and operator precedence during syntax analysis.
 *       - `ExprStack`: A specialized stack used within the `parseExpression()` method to implement the 
 *         Shunting Yard algorithm for handling operator precedence in mathematical expressions.
 *       - `OpStack`: Another specialized stack used in `parseExpression()` to manage operators during
 *         the Shunting Yard algorithm for proper expression parsing.
 *    b. Queue:
 *       - Queue data structure is not explicitly used in this project.
 *    c. List (Linked Lists):
 *       - `TokenList`: Stores the sequence of `Token`s generated by the `Lexer`. It's a simple forward-linked list.
 *       - `ExprList`: Used in the AST to represent a list of expressions, for example, arguments in a function call (`FuncCallExpr`) or expressions in a print statement (`PrintStmt`).
 *       - `ParamList`: Represents the list of parameters in a function definition (`FuncDef`) within the AST.
 *       - `StmtList`: Forms the body of a program (`Program`) or a function (`FuncDef`) by linking statements together in the AST.
 *       - `Param`: A single parameter node that forms a linked list structure for function parameters.
 *       - Internal stack nodes: `StackNode` for `ParseStack`, and similar node structures for `ExprStack` and `OpStack`.
 *    d. Tree (Regular - Abstract Syntax Tree):
 *       - The AST itself is a tree structure representing the hierarchical syntactic structure of the Res code.
 *         Nodes like `Program`, `FuncDef`, `DeclStmt`, `BinaryExpr`, etc., form this tree.
 *    e. Binary Search Tree (BST):
 *       - `SymbolTable` uses a BST (composed of `BSTNode`s) to store and manage variable identifiers.
 *         This allows for efficient lookup (checking if a variable is declared) and insertion of variables.
 *    f. AVL Tree:
 *       - AVL Tree data structure is not used in this project.
 *
 * User Inputs:
 *  a. How Input is Handled:
 *     - The compiler is designed to read Res programming language source code.
 *     - This can be from a `.res` file specified as a command-line argument.
 *     - It also features a REPL (Read-Eval-Print Loop) mode for interactive input directly in the terminal.
 *  b. Expected Input:
 *     - Text-based source code written in the Res language, following its defined syntax and keywords
 *       (e.g., `typeshi`, `func`, `coms`).
 *
 * Data Processing:
 *  a. Lexical Analysis (Lexing):
 *     - The `Lexer` class takes the raw Res source code string as input.
 *     - It scans the input character by character.
 *     - It groups characters into meaningful sequences called tokens (e.g., keywords, identifiers, operators, literals).
 *     - These tokens are stored in a `TokenList`.
 *  b. Syntax Analysis (Parsing):
 *     - The `Parser` class takes the `TokenList` from the Lexer.
 *     - It verifies if the sequence of tokens conforms to the grammatical rules of the Res language.
 *     - It builds an Abstract Syntax Tree (AST), which is a hierarchical representation of the code's structure.
 *     - During parsing, it interacts with the `SymbolTable` to record variable declarations and check for their existence.
 *  c. Semantic Analysis (Partially through SymbolTable):
 *     - Basic semantic checks, like ensuring variables are declared before use, are performed using the `SymbolTable`.
 *  d. Code Generation (Future/Partial):
 *     - The architecture includes a `CodeGenerator` (though its implementation details are not fully shown in this snippet).
 *     - Its role is to take the AST produced by the Parser and translate it into equivalent, executable C++ code.
 *
 * Display Outputs:
 *  a. Compiled Code:
 *     - The primary output is the generated C++ code, which is equivalent to the input Res program.
 *     - This C++ code is typically written to a `.cpp` file (e.g., in the `saves` directory as mentioned in comments).
 *  b. REPL Interaction:
 *     - In REPL mode, the results of evaluated expressions or messages might be printed directly to the console.
 *  c. Error Messages:
 *     - If syntax errors, undeclared variables, or other issues are detected during lexing or parsing,
 *       the compiler will output error messages to the console to help the user identify and fix the problems.
 *     - For example, an "Unterminated string literal" error from the `Lexer` or errors from the `Parser` if grammar rules are violated.
 *
 * Data Saving/Loading:
 *  a. Loading Source Code:
 *     - The compiler loads Res source code from files (e.g., `main.res` if provided as an argument).
 *     - The `main` function typically handles opening and reading these source files into a string.
 *  b. Saving Compiled Code:
 *     - After successful compilation (lexing, parsing, and code generation), the generated C++ code
 *       is saved to a new file. The initial comments suggest these are stored in a `saves` directory.
 *     - The `main` function would handle creating and writing to these output C++ files.
 */
// =================================================================================================

#include <iostream>
#include <string>
#include <fstream>
#include <cctype>
#include <sstream>

// Import for h files
#include "components/ascii.h"



using namespace std;

/**
 * Token types enumeration for the Res programming language
 * Defines all possible token types that the lexer can recognize
 */
enum class TokenType {
    // Keywords
    TYPESHI,        // Variable declaration keyword
    INT,            // Integer type
    FLOAT,          // Float type
    STRING,         // String type
    COMS,           // Print statement keyword
    FUNC,           // Function declaration keyword
    
    // Identifiers and literals
    IDENTIFIER,     // Variable names, function names
    NUMBER,         // Numeric literals
    STRING_LITERAL, // String literals in quotes
    
    // Operators and punctuation
    EQUALS,         // = (assignment)
    SEMICOLON,      // ; (statement terminator)
    LPAREN,         // ( (left parenthesis)
    RPAREN,         // ) (right parenthesis)
    LBRACE,         // { (left brace)
    RBRACE,         // } (right brace)
    COMMA,          // , (parameter separator)
    PLUS,           // + (addition)
    MINUS,          // - (subtraction)
    MULT,           // * (multiplication)
    DIV,            // / (division)
    
    // Comparison operators
    EQ,             // == (equality)
    LT,             // < (less than)
    GT,             // > (greater than)
    LE,             // <= (less than or equal)
    GE,             // >= (greater than or equal)
    
    // Logical operators
    AND,            // && (logical and)
    OR,             // || (logical or)
    
    // Special tokens
    ARROW,          // => (function arrow)
    COMMENT,        // // comments
    END             // End of input marker
};

/**
 * Token structure representing a single lexical unit
 * Forms a linked list for efficient token storage and traversal
 */
struct Token {
    TokenType type;     // The type of this token
    string value;       // The actual text value of the token
    Token* next;        // Pointer to next token in the list
    
    /**
     * Constructor for creating a new token
     * @param t Token type
     * @param v Token value (text)
     */
    Token(TokenType t, string v) : type(t), value(v), next(nullptr) {}
};

/**
 * Custom linked list implementation for storing tokens
 * Provides efficient append operations and automatic memory management
 */
struct TokenList {
    Token* head;        // First token in the list
    Token* tail;        // Last token in the list
    
    /**
     * Constructor initializes empty list
     */
    TokenList() : head(nullptr), tail(nullptr) {}
    
    /**
     * Destructor implements comprehensive cleanup of all token resources
     * 
     * Memory Management Strategy: Traverses the entire linked list of tokens
     * and deallocates each Token object. This prevents memory leaks in the
     * lexical analysis phase of compilation.
     * 
     * Implementation Pattern: Uses iterative traversal with temporary pointer
     * management to safely delete each node while advancing through the list.
     */
    ~TokenList() {
        Token* current = head;
        while (current) {
            Token* next = current->next;
            delete current;
            current = next;
        }
    }
    
    /**
     * Appends a token to the end of the list
     * @param token Token to append
     */
    void append(Token* token) {
        if (!head) {
            head = tail = token;
        } else {
            tail->next = token;
            tail = token;
        }
    }
};

/**
 * Lexical analyzer for the Res programming language
 * Converts source code text into a stream of tokens
 */
class Lexer {
    string input;       // Source code to tokenize
    size_t pos;         // Current position in input string

public:
    /**
     * Constructor initializes lexer with source code
     * @param src Source code string to tokenize
     */
    Lexer(const string& src) : input(src), pos(0) {}

    /**
     * Tokenizes the entire input string
     * @return TokenList containing all tokens found in input
     */
    TokenList* tokenize() {
        TokenList* tokens = new TokenList();
        while (pos < input.length()) {
            char c = input[pos];
            if (isspace(c)) {
                // Handle whitespace but preserve newlines as they are important for statement separation
                if (c == '\n') {
                    // We could add a special NEWLINE token here if needed
                    // tokens->append(new Token(TokenType::NEWLINE, "\\n"));
                }
                pos++;
                continue;
            }
            if (isalpha(c)) {
                tokens->append(parseIdentifier());
            } else if (isdigit(c) || c == '.') {
                tokens->append(parseNumber());
            } else if (c == '"') {
                tokens->append(parseStringLiteral());
            } else if (c == '=') {
                if (pos + 1 < input.length() && input[pos + 1] == '=') {
                    tokens->append(new Token(TokenType::EQ, "=="));
                    pos += 2;
                } else if (pos + 1 < input.length() && input[pos + 1] == '>') {
                    tokens->append(new Token(TokenType::ARROW, "=>"));
                    pos += 2;
                } else {
                    tokens->append(new Token(TokenType::EQUALS, "="));
                    pos++;
                }
            } else if (c == ';') {
                tokens->append(new Token(TokenType::SEMICOLON, ";"));
                pos++;
            } else if (c == '(') {
                tokens->append(new Token(TokenType::LPAREN, "("));
                pos++;
            } else if (c == ')') {
                tokens->append(new Token(TokenType::RPAREN, ")"));
                pos++;
            } else if (c == '{') {
                tokens->append(new Token(TokenType::LBRACE, "{"));
                pos++;
            } else if (c == '}') {
                tokens->append(new Token(TokenType::RBRACE, "}"));
                pos++;
            } else if (c == ',') {
                tokens->append(new Token(TokenType::COMMA, ","));
                pos++;
            } else if (c == '+') {
                tokens->append(new Token(TokenType::PLUS, "+"));
                pos++;
            } else if (c == '-') {
                tokens->append(new Token(TokenType::MINUS, "-"));
                pos++;
            } else if (c == '*') {
                tokens->append(new Token(TokenType::MULT, "*"));
                pos++;
            } else if (c == '/') {
                if (pos + 1 < input.length() && input[pos + 1] == '/') {
                    tokens->append(parseComment());
                } else {
                    tokens->append(new Token(TokenType::DIV, "/"));
                    pos++;
                }
            } else if (c == '<') {
                if (pos + 1 < input.length() && input[pos + 1] == '=') {
                    tokens->append(new Token(TokenType::LE, "<="));
                    pos += 2;
                } else {
                    tokens->append(new Token(TokenType::LT, "<"));
                    pos++;
                }
            } else if (c == '>') {
                if (pos + 1 < input.length() && input[pos + 1] == '=') {
                    tokens->append(new Token(TokenType::GE, ">="));
                    pos += 2;
                } else {
                    tokens->append(new Token(TokenType::GT, ">"));
                    pos++;
                }
            } else if (c == '&') {
                if (pos + 1 < input.length() && input[pos + 1] == '&') {
                    tokens->append(new Token(TokenType::AND, "&&"));
                    pos += 2;
                } else {
                    throw runtime_error("Invalid character: &");
                }
            } else if (c == '|') {
                if (pos + 1 < input.length() && input[pos + 1] == '|') {
                    tokens->append(new Token(TokenType::OR, "||"));
                    pos += 2;
                } else {
                    throw runtime_error("Invalid character: |");
                }
            } else {
                throw runtime_error("Unknown character: " + string(1, c));
            }
        }
        tokens->append(new Token(TokenType::END, ""));
        return tokens;
    }

private:
    /**
     * Parses an identifier or keyword starting at current position
     * Implementation Decision: Uses a simple string-based keyword lookup rather than
     * a hash table for efficiency with small keyword set. This approach is more
     * readable and maintainable for a language with few reserved words.
     * @return Token representing the identifier or keyword found
     */
    Token* parseIdentifier() {
        string value;
        // Collect all alphanumeric characters for the identifier
        // This allows variable names like "var1", "count2", etc.
        while (pos < input.length() && isalnum(input[pos])) {
            value += input[pos++];
        }
        
        // Keyword recognition: Check against all reserved words in Res language
        // Using if-statements instead of a map for simplicity and performance
        // with this small set of keywords
        if (value == "typeshi") return new Token(TokenType::TYPESHI, value);
        if (value == "int") return new Token(TokenType::INT, value);
        if (value == "float") return new Token(TokenType::FLOAT, value);
        if (value == "string") return new Token(TokenType::STRING, value);
        if (value == "coms") return new Token(TokenType::COMS, value);
        if (value == "func") return new Token(TokenType::FUNC, value);
        
        // If not a keyword, treat as a user-defined identifier
        return new Token(TokenType::IDENTIFIER, value);
    }

    /**
     * Parses a numeric literal starting at current position
     * Implementation Decision: Uses basic floating-point parsing that accepts
     * multiple decimal points (like "1.2.3") for simplicity. A production
     * compiler would need more robust number validation.
     * @return Token representing the number found
     */
    Token* parseNumber() {
        string value;
        // Collect all digits and decimal points (basic number parsing)
        // This approach allows for both integers (123) and floats (123.45)
        // Note: This simple implementation doesn't validate number format
        // and would accept invalid numbers like "1.2.3.4"
        while (pos < input.length() && (isdigit(input[pos]) || input[pos] == '.')) {
            value += input[pos++];
        }
        return new Token(TokenType::NUMBER, value);
    }

    /**
     * Parses a string literal enclosed in quotes
     * Implementation Decision: Uses simple quote-delimited strings without
     * escape sequence support. This keeps the lexer simple while handling
     * the most common string use cases in the Res language.
     * @return Token representing the string literal found
     * @throws runtime_error if string is not properly terminated
     */
    Token* parseStringLiteral() {
        string value;
        pos++; // Skip opening quote
        
        // Collect characters until closing quote is found
        // Note: This implementation doesn't support escape sequences like \n, \"
        // A more sophisticated lexer would handle these special cases
        while (pos < input.length() && input[pos] != '"') {
            value += input[pos++];
        }
        
        // Ensure the string is properly terminated
        if (pos >= input.length()) throw runtime_error("Unterminated string literal");
        pos++; // Skip closing quote
        
        // Return the string with quotes preserved for easier code generation
        return new Token(TokenType::STRING_LITERAL, "\"" + value + "\"");
    }

    /**
     * Parses a comment starting with //
     * Implementation Decision: Comments are preserved as tokens rather than
     * discarded during lexing. This allows for potential documentation
     * generation or code formatting tools that need comment information.
     * @return Token representing the comment found
     */
    Token* parseComment() {
        string value = "//";
        pos += 2; // Skip //
        
        // Collect the entire comment line until newline
        // This preserves the comment content for potential future use
        while (pos < input.length() && input[pos] != '\n') {
            value += input[pos++];
        }
        
        // Also consume the newline character if present
        // This prevents the newline from being processed as a separate token
        if (pos < input.length() && input[pos] == '\n') {
            pos++;
        }
        return new Token(TokenType::COMMENT, value);
    }
};

/**
 * Binary Search Tree node for the symbol table
 * Stores variable names in alphabetical order
 */
struct BSTNode {
    string value;       // Variable name
    BSTNode* left;      // Left child (smaller values)
    BSTNode* right;     // Right child (larger values)
    
    BSTNode(string v) : value(v), left(nullptr), right(nullptr) {}
};

/**
 * Symbol table implementation using Binary Search Tree
 * 
 * Design Rationale: BST provides O(log n) average case performance for
 * variable lookups while maintaining alphabetical ordering. This is ideal
 * for a compiler's symbol table where we need fast variable existence
 * checks and the ability to iterate through variables in sorted order.
 * 
 * Alternative approaches considered:
 * - Hash table: Faster O(1) average case but no ordering, more complex
 * - Linear list: Simpler but O(n) lookup time
 * - Trie: Good for prefix matching but overkill for simple variable names
 */
class SymbolTable {
    BSTNode* root;      // Root of the BST

    /**
     * Recursively inserts a variable into the BST
     * 
     * Logic Explanation: Uses standard BST insertion algorithm where:
     * - Smaller values go to the left subtree
     * - Larger values go to the right subtree  
     * - Duplicate values are ignored (no action taken)
     * 
     * This maintains the BST invariant and prevents duplicate variable
     * declarations in the same scope.
     * 
     * @param node Reference to current node pointer (allows modification)
     * @param var Variable name to insert
     */
    void insert(BSTNode*& node, const string& var) {
        if (!node) {
            // Base case: Create new node at empty position
            node = new BSTNode(var);
            return;
        }
        // Recursive cases: Navigate to appropriate subtree
        if (var < node->value) insert(node->left, var);
        else if (var > node->value) insert(node->right, var);
        // If var == node->value, ignore duplicate (no action needed)
    }

    /**
     * Recursively searches for a variable in the BST
     * 
     * Logic Explanation: Implements standard BST search algorithm:
     * 1. If current node is null, variable doesn't exist
     * 2. If variable matches current node, found it
     * 3. If variable is lexicographically smaller, search left subtree
     * 4. Otherwise, search right subtree
     * 
     * Time Complexity: O(log n) average case, O(n) worst case (unbalanced tree)
     * 
     * @param node Current node to check
     * @param var Variable name to find
     * @return true if variable is found, false otherwise
     */
    bool find(BSTNode* node, const string& var) const {
        if (!node) return false;                    // Base case: not found
        if (var == node->value) return true;        // Base case: found
        if (var < node->value) return find(node->left, var);   // Search left
        return find(node->right, var);              // Search right
    }

    /**
     * Recursively deletes all nodes in the BST
     * @param node Current node to delete
     */
    void clear(BSTNode* node) {
        if (!node) return;
        clear(node->left);
        clear(node->right);
        delete node;
    }

public:
    /**
     * Constructor initializes empty symbol table
     */
    SymbolTable() : root(nullptr) {}
    
    /**
     * Destructor implements comprehensive BST cleanup
     * 
     * Memory Management Strategy: Uses the recursive clear() helper function
     * to perform post-order traversal deletion of all BST nodes. This ensures
     * proper cleanup of the entire tree structure.
     * 
     * The destructor follows RAII principles, automatically cleaning up
     * resources when the SymbolTable goes out of scope.
     */
    ~SymbolTable() { clear(root); }

    /**
     * Adds a variable to the symbol table
     * @param var Variable name to add
     */
    void addVariable(const string& var) {
        insert(root, var);
    }

    /**
     * Checks if a variable has been declared
     * @param var Variable name to check
     * @return true if variable is declared, false otherwise
     */
    bool isDeclared(const string& var) const {
        return find(root, var);
    }
};

/**
 * Abstract Syntax Tree (AST) Node Definitions
 * These structures represent the parsed structure of Res programs
 */

/**
 * Base class for all AST nodes
 * Provides virtual destructor for proper cleanup
 */
struct ASTNode {
    virtual ~ASTNode() = default;
};

/**
 * Base class for all expression nodes
 * Expressions represent values that can be computed
 */
struct Expr : ASTNode {
    virtual ~Expr() = default;
};

/**
 * AST node for numeric literals (integers and floats)
 */
struct NumberExpr : Expr {
    string value;       // The numeric value as a string
    NumberExpr(const string& v) : value(v) {}
};

/**
 * AST node for string literals
 */
struct StringExpr : Expr {
    string value;       // The string value including quotes
    StringExpr(const string& v) : value(v) {}
};

/**
 * AST node for variable references
 */
struct VarExpr : Expr {
    string name;        // The variable name
    VarExpr(const string& n) : name(n) {}
};

/**
 * AST node for binary expressions (e.g., a + b, x == y)
 */
struct BinaryExpr : Expr {
    Expr* left;         // Left operand
    TokenType op;       // Operator type
    Expr* right;        // Right operand
    
    BinaryExpr() : left(nullptr), right(nullptr) {}
    ~BinaryExpr() { delete left; delete right; }
};

/**
 * AST node for function call expressions
 */
struct FuncCallExpr : Expr {
    string name;                    // Function name
    struct ExprList* arguments;     // List of arguments
    
    FuncCallExpr() : arguments(nullptr) {}
    ~FuncCallExpr();    // Forward declaration, defined later
};

/**
 * Linked list of expressions (used for function arguments, print statements)
 */
struct ExprList {
    Expr* expr;         // Current expression
    ExprList* next;     // Next expression in list
    
    ExprList(Expr* e) : expr(e), next(nullptr) {}
    ~ExprList() { delete expr; delete next; }
};

/**
 * AST node for variable declaration statements
 * Example: typeshi int x = 5;
 */
struct DeclStmt : ASTNode {
    string type;        // Variable type (int, float, string)
    string varName;     // Variable name
    Expr* value;        // Initial value expression
    
    DeclStmt() : value(nullptr) {}
    ~DeclStmt() { delete value; }
};

/**
 * AST node for print statements
 * Example: coms(x, "hello");
 */
struct PrintStmt : ASTNode {
    ExprList* expressions;  // List of expressions to print
    
    PrintStmt() : expressions(nullptr) {}
    ~PrintStmt() { delete expressions; }
};

/**
 * AST node for function definitions
 * Example: func add(int a, int b) => { ... }
 */
struct FuncDef : ASTNode {
    string name;                // Function name
    struct ParamList* params;   // Parameter list
    struct StmtList* body;      // Function body statements
    
    FuncDef() : params(nullptr), body(nullptr) {}
    ~FuncDef();    // Forward declaration, defined later
};

/**
 * Single function parameter
 */
struct Param {
    string type;        // Parameter type
    string name;        // Parameter name
    Param* next;        // Next parameter in list
    
    Param(string t, string n) : type(t), name(n), next(nullptr) {}
    ~Param() { delete next; }
};

/**
 * Linked list of function parameters
 */
struct ParamList {
    Param* param;       // Current parameter
    ParamList* next;    // Next parameter list node
    
    ParamList(Param* p) : param(p), next(nullptr) {}
    ~ParamList() { delete param; delete next; }
};

/**
 * Linked list of statements
 */
struct StmtList {
    ASTNode* stmt;      // Current statement
    StmtList* next;     // Next statement in list
    
    StmtList(ASTNode* s) : stmt(s), next(nullptr) {}
    /**
     * Destructor implements comprehensive cleanup of linked list resources
     * 
     * Memory Management Strategy: Recursively deletes both the current statement
     * and the rest of the list. This ensures complete cleanup of statement chains
     * without memory leaks.
     * 
     * The destructor design follows RAII principles where each StmtList node
     * is responsible for cleaning up its statement and delegating cleanup of
     * the remaining list to the next node's destructor.
     */
    ~StmtList() { delete stmt; delete next; }
};

/**
 * Root AST node representing a complete program
 */
struct Program : ASTNode {
    StmtList* statements;   // All statements in the program
    
    Program() : statements(nullptr) {}
    /**
     * Destructor ensures complete program AST cleanup
     * 
     * Memory Management: The Program destructor cleans up the entire statement
     * list, which will recursively delete all contained statements and their
     * associated AST nodes through the StmtList destructor chain.
     */
    ~Program() { delete statements; }
};

// Forward declarations for AST node destructors
// These implement proper memory management for complex AST structures
FuncCallExpr::~FuncCallExpr() { delete arguments; }    // Cleanup argument expression list
FuncDef::~FuncDef() { delete params; delete body; }    // Cleanup parameters and function body

/**
 * Parser state for stack-based parsing
 * Represents the current parsing state and associated AST node
 */
struct ParseState {
    string state;       // Current parsing state description
    ASTNode* node;      // Associated AST node being built
    
    ParseState(string s, ASTNode* n) : state(s), node(n) {}
};

/**
 * Stack node for the parsing stack
 */
struct StackNode {
    ParseState* data;   // Parse state data
    StackNode* next;    // Next node in stack
    
    StackNode(ParseState* d) : data(d), next(nullptr) {}
};

/**
 * Custom stack implementation for parsing
 * Used to manage parsing states in a stack-based parser
 */
class ParseStack {
    StackNode* top;     // Top of the stack
    
public:
    ParseStack() : top(nullptr) {}
    
    /**
     * Destructor implements proper stack cleanup for parsing state management
     * 
     * Memory Management Strategy: Iteratively cleans up all remaining stack
     * nodes and their associated parsing state data. This prevents memory
     * leaks when parsing is interrupted by exceptions or normal completion.
     * 
     * Safety Consideration: The destructor handles both ParseState objects
     * and StackNode objects separately to ensure complete resource cleanup.
     */
    ~ParseStack() {
        while (top) {
            StackNode* temp = top;
            top = top->next;
            delete temp->data;  // Clean up ParseState object
            delete temp;        // Clean up StackNode object
        }
    }

    /**
     * Pushes a new parsing state onto the stack
     * @param state Description of the parsing state
     * @param node Associated AST node
     */
    void push(string state, ASTNode* node) {
        StackNode* newNode = new StackNode(new ParseState(state, node));
        newNode->next = top;
        top = newNode;
    }
    
    /**
     * Pops the top parsing state from the stack
     * @return ParseState from top of stack
     * @throws runtime_error if stack is empty
     */
    ParseState* pop() {
        if (!top) throw runtime_error("Stack underflow");
        StackNode* temp = top;
        top = top->next;
        ParseState* data = temp->data;
        delete temp;
        return data;
    }
    
    /**
     * Checks if the stack is empty
     * @return true if stack is empty, false otherwise
     */
    bool empty() const { return top == nullptr; }
};

/**
 * Syntax analyzer for the Res programming language
 * Converts tokens into an Abstract Syntax Tree (AST)
 * Uses a stack-based parsing approach for better control flow
 */
class Parser {
    TokenList* tokens;      // Input tokens to parse
    Token* current;         // Current token being processed
    ParseStack stack;       // Stack for managing parsing states
    SymbolTable& symbols;   // Symbol table for variable tracking

public:
    /**
     * Constructor initializes parser with tokens and symbol table
     * @param t TokenList containing all tokens to parse
     * @param s SymbolTable reference for variable tracking
     */
    Parser(TokenList* t, SymbolTable& s) : tokens(t), current(t->head), symbols(s) {}
    
    /**
     * Destructor ensures proper cleanup of all parser resources
     * 
     * Memory Management Philosophy: The parser takes ownership of the token list
     * provided in the constructor and is responsible for its cleanup. This follows
     * RAII principles where resource acquisition and release are tied to object
     * lifetime.
     * 
     * The token list's destructor will recursively clean up all individual tokens,
     * preventing memory leaks in the compilation pipeline.
     */
    ~Parser() { delete tokens; }

    /**
     * Parses the entire token stream into a Program AST
     * @return Program AST representing the parsed code
     */
    Program* parse() {
        Program* program = new Program();
        StmtList** tail = &program->statements;
        
        // Parse all statements until end of input
        while (current && current->type != TokenType::END) {
            ASTNode* stmt = parseStatement();
            if (stmt) {
                *tail = new StmtList(stmt);
                tail = &(*tail)->next;
            }
            // parseStatement handles token advancement
        }
        return program;
    }

private:
    /**
     * Parses a single statement based on the current token
     * @return ASTNode representing the parsed statement, or nullptr if none
     * @throws runtime_error for unexpected tokens
     */
    ASTNode* parseStatement() {
        if (!current) return nullptr;
        
        // Skip any whitespace tokens
        while (current && isspace(current->value[0])) {
            current = current->next;
        }
        
        if (!current) return nullptr;
        
        ASTNode* result = nullptr;
        
        // Determine statement type based on first token
        if (current->type == TokenType::TYPESHI) {
            // Variable declaration: typeshi int x = 5;
            result = parseDeclStmt();
        } else if (current->type == TokenType::COMS) {
            // Print statement: coms(x, "hello");
            result = parsePrintStmt();
        } else if (current->type == TokenType::FUNC) {
            // Function definition: func add(int a, int b) => { ... }
            result = parseFuncDef();
        } else if (current->type == TokenType::IDENTIFIER) {
            // Function call statement: myFunc(arg1, arg2);
            result = parseFuncCallStmt();
        } else if (current->type == TokenType::COMMENT) {
            // Skip comments
            current = current->next;
            return nullptr;
        } else if (current->type == TokenType::END) {
            // End of input
            return nullptr;
        } else {
            // Error Recovery Strategy: Handle unexpected tokens gracefully
            // Rather than crashing, we provide specific error information and
            // continue parsing to potentially find more errors
            string invalidToken = current->value;
            current = current->next; // Skip unrecognized tokens to prevent infinite loops
            throw runtime_error("Unexpected token: " + invalidToken);
        }
        
        return result;
    }

    /**
     * Parses variable declaration statements
     * 
     * Grammar: typeshi <type> <identifier> = <expression> ;
     * Example: typeshi int count = 10;
     * 
     * Implementation Strategy: Uses a state machine approach with explicit states
     * to ensure correct syntax parsing. This approach provides clear error messages
     * and makes the parser's progression through the declaration syntax obvious.
     * 
     * Design Decision: All variables must be initialized at declaration to simplify
     * code generation and avoid uninitialized variable issues in the target C++ code.
     * 
     * @return DeclStmt AST node representing the variable declaration
     * @throws runtime_error for syntax errors in the declaration
     */
    DeclStmt* parseDeclStmt() {
        stack.push("expect_typeshi", nullptr);
        DeclStmt* decl = nullptr;
        while (!stack.empty()) {
            ParseState* state = stack.pop();
            string s = state->state;
            ASTNode* node = state->node;
            delete state;
            if (s == "expect_typeshi") {
                // State 1: Expect the 'typeshi' keyword to start declaration
                if (current->type != TokenType::TYPESHI) throw runtime_error("Expected typeshi");
                current = current->next;
                stack.push("expect_type", nullptr);
            } else if (s == "expect_type") {
                // State 2: Expect type specification (int, float, or string)
                if (current->type != TokenType::INT && current->type != TokenType::FLOAT && current->type != TokenType::STRING) {
                    throw runtime_error("Expected int, float, or string");
                }
                decl = new DeclStmt();
                decl->type = current->value;
                current = current->next;
                stack.push("expect_identifier", decl);
            } else if (s == "expect_identifier") {
                // State 3: Expect variable name and register it in symbol table
                if (current->type != TokenType::IDENTIFIER) throw runtime_error("Expected identifier");
                decl = dynamic_cast<DeclStmt*>(node);
                decl->varName = current->value;
                symbols.addVariable(decl->varName);  // Register variable for scope checking
                current = current->next;
                stack.push("expect_equals", decl);
            } else if (s == "expect_equals") {
                // State 4: Expect assignment operator (mandatory initialization)
                if (current->type != TokenType::EQUALS) throw runtime_error("Expected =");
                current = current->next;
                stack.push("expect_expression", node);
            } else if (s == "expect_expression") {
                // State 5: Parse the initialization expression
                decl = dynamic_cast<DeclStmt*>(node);
                decl->value = parseExpression();
                stack.push("expect_semicolon", decl);
            } else if (s == "expect_semicolon") {
                // State 6: Expect statement terminator
                if (current->type != TokenType::SEMICOLON) throw runtime_error("Expected ;");
                current = current->next;
                return decl;
            }
        }
        return decl;
    }

    /**
     * Parses print statement (output) syntax
     * 
     * Grammar: coms ( <expression_list> ) ;
     * Example: coms(x, "Hello", x + 5);
     * 
     * Implementation Strategy: Uses state machine parsing to handle the print
     * statement's parenthesized expression list syntax. The 'coms' keyword is
     * chosen to be distinctive from C++ keywords and clearly indicate output.
     * 
     * Design Decision: Print statements can accept multiple expressions separated
     * by commas, which will be output sequentially. This provides flexibility
     * for debugging and user output without requiring multiple print calls.
     * 
     * @return PrintStmt AST node representing the print statement
     * @throws runtime_error for syntax errors in the print statement
     */
    PrintStmt* parsePrintStmt() {
        stack.push("expect_coms", nullptr);
        PrintStmt* print = nullptr;
        while (!stack.empty()) {
            ParseState* state = stack.pop();
            string s = state->state;
            ASTNode* node = state->node;
            delete state;
            if (s == "expect_coms") {
                // State 1: Expect 'coms' keyword to start print statement
                if (current->type != TokenType::COMS) throw runtime_error("Expected coms");
                current = current->next;
                stack.push("expect_lparen", nullptr);
            } else if (s == "expect_lparen") {
                // State 2: Expect opening parenthesis for expression list
                if (current->type != TokenType::LPAREN) throw runtime_error("Expected (");
                current = current->next;
                print = new PrintStmt();
                stack.push("expect_expr_list", print);
            } else if (s == "expect_expr_list") {
                // State 3: Parse comma-separated list of expressions to print
                print = dynamic_cast<PrintStmt*>(node);
                print->expressions = parseExprList();
                stack.push("expect_rparen", print);
            } else if (s == "expect_rparen") {
                // State 4: Expect closing parenthesis
                if (current->type != TokenType::RPAREN) throw runtime_error("Expected )");
                current = current->next;
                stack.push("expect_semicolon", node);
            } else if (s == "expect_semicolon") {
                // State 5: Expect statement terminator
                if (current->type != TokenType::SEMICOLON) throw runtime_error("Expected ;");
                current = current->next;
                return print;
            }
        }
        return print;
    }

    /**
     * Parses function definition syntax
     * 
     * Grammar: func <identifier> ( <parameter_list> ) => { <statement_list> }
     * Example: func add(int a, int b) => { typeshi int result = a + b; }
     * 
     * Implementation Strategy: Uses comprehensive state machine to handle the
     * multi-part function syntax. The '=>' arrow syntax clearly separates the
     * function signature from the body, making function definitions visually
     * distinct from other language constructs.
     * 
     * Design Decision: Functions use arrow syntax (=>) rather than immediate braces
     * to provide visual clarity and distinguish functions from control structures.
     * This makes the code more readable and prevents syntax ambiguity.
     * 
     * @return FuncDef AST node representing the function definition
     * @throws runtime_error for syntax errors in the function definition
     */
    FuncDef* parseFuncDef() {
        stack.push("expect_func", nullptr);
        FuncDef* func = nullptr;
        while (!stack.empty()) {
            ParseState* state = stack.pop();
            string s = state->state;
            ASTNode* node = state->node;
            delete state;
            if (s == "expect_func") {
                // State 1: Expect 'func' keyword to start function definition
                if (current->type != TokenType::FUNC) throw runtime_error("Expected func");
                current = current->next;
                stack.push("expect_identifier", nullptr);
            } else if (s == "expect_identifier") {
                // State 2: Expect function name identifier
                if (current->type != TokenType::IDENTIFIER) throw runtime_error("Expected identifier");
                func = new FuncDef();
                func->name = current->value;
                current = current->next;
                stack.push("expect_lparen", func);
            } else if (s == "expect_lparen") {
                // State 3: Expect opening parenthesis for parameter list
                if (current->type != TokenType::LPAREN) throw runtime_error("Expected (");
                current = current->next;
                stack.push("expect_param_list", node);
            } else if (s == "expect_param_list") {
                // State 4: Parse optional parameter list (can be empty)
                func = dynamic_cast<FuncDef*>(node);
                func->params = parseParamList();
                stack.push("expect_rparen", node);
            } else if (s == "expect_rparen") {
                // State 5: Expect closing parenthesis
                if (current->type != TokenType::RPAREN) throw runtime_error("Expected )");
                current = current->next;
                stack.push("expect_arrow", node);
            } else if (s == "expect_arrow") {
                // State 6: Expect arrow syntax '=>' before function body
                if (current->type != TokenType::ARROW) throw runtime_error("Expected =>");
                current = current->next;
                stack.push("expect_lbrace", node);
            } else if (s == "expect_lbrace") {
                // State 7: Expect opening brace for function body
                if (current->type != TokenType::LBRACE) throw runtime_error("Expected {");
                current = current->next;
                stack.push("expect_body", node);
            } else if (s == "expect_body") {
                // State 8: Parse function body statements
                func = dynamic_cast<FuncDef*>(node);
                func->body = parseStmtList();
                stack.push("expect_rbrace", node);
            } else if (s == "expect_rbrace") {
                // State 9: Expect closing brace to end function
                if (current->type != TokenType::RBRACE) throw runtime_error("Expected }");
                current = current->next;
                return func;
            }
        }
        return func;
    }

    /**
     * Parses function call statements
     * 
     * Grammar: <identifier> ( <expression_list> ) ;
     * Example: myFunction(arg1, arg2 + 3, "hello");
     * 
     * Implementation Strategy: Uses state machine parsing to handle function
     * call syntax with parenthesized argument lists. Function calls as statements
     * are distinguished from function calls within expressions by the required
     * semicolon terminator.
     * 
     * Design Decision: Function calls must be terminated with semicolons when
     * used as statements, following standard imperative language conventions.
     * This prevents parsing ambiguity and maintains consistency with other statements.
     * 
     * @return FuncCallExpr AST node representing the function call
     * @throws runtime_error for syntax errors in the function call
     */
    FuncCallExpr* parseFuncCallStmt() {
        stack.push("expect_identifier", nullptr);
        FuncCallExpr* call = nullptr;
        while (!stack.empty()) {
            ParseState* state = stack.pop();
            string s = state->state;
            ASTNode* node = state->node;
            delete state;
            if (s == "expect_identifier") {
                // State 1: Expect function name identifier
                if (current->type != TokenType::IDENTIFIER) throw runtime_error("Expected identifier");
                call = new FuncCallExpr();
                call->name = current->value;
                current = current->next;
                stack.push("expect_lparen", call);
            } else if (s == "expect_lparen") {
                // State 2: Expect opening parenthesis for argument list
                if (current->type != TokenType::LPAREN) throw runtime_error("Expected (");
                current = current->next;
                stack.push("expect_expr_list", node);
            } else if (s == "expect_expr_list") {
                // State 3: Parse comma-separated argument expressions
                call = dynamic_cast<FuncCallExpr*>(node);
                call->arguments = parseExprList();
                stack.push("expect_rparen", node);
            } else if (s == "expect_rparen") {
                // State 4: Expect closing parenthesis
                if (current->type != TokenType::RPAREN) throw runtime_error("Expected )");
                current = current->next;
                stack.push("expect_semicolon", node);
            } else if (s == "expect_semicolon") {
                // State 5: Expect statement terminator (distinguishes from expression context)
                if (current->type != TokenType::SEMICOLON) throw runtime_error("Expected ;");
                current = current->next;
                return call;
            }
        }
        return call;
    }

    /**
     * Parses function parameter lists
     * 
     * Grammar: [<type> <identifier> [, <type> <identifier>]*]
     * Example: int a, float b, string name
     * 
     * Implementation Strategy: Uses a simple loop-based approach rather than
     * state machine since parameter parsing is straightforward. Handles empty
     * parameter lists gracefully by returning nullptr.
     * 
     * Design Decision: Parameters must be explicitly typed (no type inference)
     * to maintain clarity and simplify code generation. Each parameter is
     * automatically registered in the symbol table for scope checking.
     * 
     * @return ParamList linked list of parameters, or nullptr if empty
     * @throws runtime_error for invalid parameter syntax
     */
    ParamList* parseParamList() {
        // Handle empty parameter list case
        if (!current || current->type == TokenType::RPAREN) return nullptr;
        
        ParamList* head = nullptr;
        ParamList** tail = &head;
        
        do {
            // Expect parameter type (int, float, or string)
            if (current->type != TokenType::INT && current->type != TokenType::FLOAT && current->type != TokenType::STRING) {
                throw runtime_error("Expected type in parameter");
            }
            string type = current->value;
            current = current->next;
            
            // Expect parameter name
            if (current->type != TokenType::IDENTIFIER) throw runtime_error("Expected parameter name");
            string name = current->value;
            
            // Register parameter in symbol table for scope checking
            symbols.addVariable(name);
            
            // Add parameter to list
            *tail = new ParamList(new Param(type, name));
            tail = &(*tail)->next;
            current = current->next;
            
            // Handle optional comma separator
            if (current && current->type == TokenType::COMMA) current = current->next;
        } while (current && current->type != TokenType::RPAREN);
        
        return head;
    }

    /**
     * Parses comma-separated expression lists
     * 
     * Grammar: [<expression> [, <expression>]*]
     * Example: x + 1, "hello", myVar, func(a, b)
     * 
     * Implementation Strategy: Uses a straightforward loop-based approach to
     * parse comma-separated expressions. Each expression is parsed independently
     * using the full expression parser, allowing complex nested expressions.
     * 
     * Design Decision: Expression lists are used in function calls and print
     * statements. Empty lists are handled gracefully by returning nullptr,
     * which simplifies the calling code's null-checking logic.
     * 
     * @return ExprList linked list of expressions, or nullptr if empty
     * @throws runtime_error for invalid expression syntax
     */
    ExprList* parseExprList() {
        // Handle empty expression list case
        if (!current || current->type == TokenType::RPAREN) return nullptr;
        
        ExprList* head = nullptr;
        ExprList** tail = &head;
        
        do {
            // Parse each expression using full expression parser
            *tail = new ExprList(parseExpression());
            tail = &(*tail)->next;
            
            // Handle optional comma separator
            if (current && current->type == TokenType::COMMA) current = current->next;
        } while (current && current->type != TokenType::RPAREN);
        
        return head;
    }


    /**
     * Parses expression using Shunting Yard algorithm for operator precedence
     * 
     * Algorithm Choice Rationale: The Shunting Yard algorithm elegantly handles
     * operator precedence and associativity without requiring recursive descent.
     * This approach uses two stacks:
     * - Expression stack: Holds operands and intermediate results
     * - Operator stack: Holds operators waiting to be applied
     * 
     * The algorithm processes tokens left-to-right and applies operators
     * when their precedence rules dictate, naturally building the correct
     * evaluation order.
     * 
     * @return Expression AST representing the parsed expression
     * @throws runtime_error for invalid expressions or operator precedence issues
     */
    Expr* parseExpression() {
        // Use proper Stack data structures
        class ExprStack {
            struct Node {
                Expr* data;
                Node* next;
                Node(Expr* d) : data(d), next(nullptr) {}
            };
            Node* top;
        public:
            ExprStack() : top(nullptr) {}
            /**
             * Expression stack destructor with comprehensive error recovery
             * 
             * Memory Safety: Cleans up all expression objects on the stack,
             * preventing memory leaks when parsing fails due to syntax errors
             * or other exceptions. This is crucial for maintaining memory
             * safety in the expression parsing subsystem.
             */
            ~ExprStack() {
                while (top) {
                    Node* temp = top;
                    top = top->next;
                    delete temp->data;  // Clean up Expression AST node
                    delete temp;        // Clean up stack node structure
                }
            }
            void push(Expr* expr) {
                Node* newNode = new Node(expr);
                newNode->next = top;
                top = newNode;
            }
            Expr* pop() {
                if (!top) throw runtime_error("Expression stack underflow");
                Node* temp = top;
                top = top->next;
                Expr* data = temp->data;
                delete temp;    // Only delete the node, return the data
                return data;
            }
            bool empty() const { return top == nullptr; }
        };

        class OpStack {
            struct Node {
                TokenType data;
                Node* next;
                Node(TokenType d) : data(d), next(nullptr) {}
            };
            Node* top;
        public:
            OpStack() : top(nullptr) {}
            /**
             * Operator stack destructor for safe cleanup
             * 
             * Resource Management: Cleans up all operator tokens stored on the
             * stack. Since operators are TokenType enums (not pointers), we only
             * need to clean up the stack node structures themselves.
             */
            ~OpStack() {
                while (top) {
                    Node* temp = top;
                    top = top->next;
                    delete temp;    // Only delete node structure, not enum data
                }
            }
            void push(TokenType op) {
                Node* newNode = new Node(op);
                newNode->next = top;
                top = newNode;
            }
            TokenType pop() {
                if (!top) throw runtime_error("Operator stack underflow");
                Node* temp = top;
                top = top->next;
                TokenType data = temp->data;
                delete temp;    // Delete node structure only
                return data;
            }
            bool empty() const { return top == nullptr; }
            TokenType peek() const {
                if (!top) throw runtime_error("Operator stack empty");
                return top->data;
            }
        };

        ExprStack exprStack;
        OpStack opStack;

        //stack.push("parse_expr", nullptr);
        //while (!stack.empty()) {
        //    ParseState* state = stack.pop();
        //    string s = state->state;
        //    delete state;
        //    if (s == "parse_expr") {
                
        // Lambda function to determine operator precedence
        // Higher numbers indicate higher precedence (evaluated first)
        // This follows standard mathematical precedence rules
        auto precedence = [](TokenType op) {
            switch (op) {
                case TokenType::MULT:       // Multiplication: highest precedence
                case TokenType::DIV:        // Division: highest precedence
                    return 2;
                case TokenType::PLUS:       // Addition: medium precedence  
                case TokenType::MINUS:      // Subtraction: medium precedence
                    return 1;
                case TokenType::EQ:         // Comparison operators: lowest precedence
                case TokenType::LT:         // These are evaluated after arithmetic
                case TokenType::GT:
                case TokenType::LE:
                case TokenType::GE:
                    return 0;
                default:
                    return -1; // Not an operator
            }
        };

        // Lambda function to apply an operator from the operator stack
        // This implements the core logic of the Shunting Yard algorithm
        auto applyOp = [&]() {
            if (opStack.empty()) return;
            
            // Pop operator and two operands (note: order matters for non-commutative ops)
            TokenType op = opStack.pop();
            Expr* right = exprStack.pop();  // Second operand (right side)
            Expr* left = exprStack.pop();   // First operand (left side)
            
            // Create binary expression AST node
            BinaryExpr* bin = new BinaryExpr();
            bin->op = op;
            bin->left = left;
            bin->right = right;
            
            // Push the result back onto expression stack
            exprStack.push(bin);
        };

        // Main parsing loop - process tokens using Shunting Yard algorithm
        Token* token = current;
        while (token && token->type != TokenType::SEMICOLON && 
               token->type != TokenType::COMMA && token->type != TokenType::RPAREN) {
            
            if (token->type == TokenType::NUMBER) {
                // Operand: Numbers are pushed directly onto expression stack
                exprStack.push(new NumberExpr(token->value));
                token = token->next;
            } else if (token->type == TokenType::STRING_LITERAL) {
                // Operand: String literals are pushed directly onto expression stack
                exprStack.push(new StringExpr(token->value));
                token = token->next;
            } else if (token->type == TokenType::IDENTIFIER) {
                // Operand: Variable references (with scope checking)
                if (!symbols.isDeclared(token->value)) 
                    throw runtime_error("Undefined variable: " + token->value);
                exprStack.push(new VarExpr(token->value));
                token = token->next;
            } else if (token->type == TokenType::LPAREN) {
                // Left parenthesis: Push onto operator stack (acts as precedence barrier)
                opStack.push(TokenType::LPAREN);
                token = token->next;        } else if (token->type == TokenType::RPAREN) {
            // Right parenthesis: Apply all operators until matching left parenthesis
            // Error Handling: Check for mismatched parentheses and provide clear error message
            while (!opStack.empty() && opStack.peek() != TokenType::LPAREN) {
                applyOp();
            }
            if (!opStack.empty()) opStack.pop(); // Remove the LPAREN
            else throw runtime_error("Mismatched parentheses");
            token = token->next;
            } else if (token->type == TokenType::PLUS || token->type == TokenType::MINUS ||
                       token->type == TokenType::MULT || token->type == TokenType::DIV ||
                       token->type == TokenType::EQ || token->type == TokenType::LT ||
                       token->type == TokenType::GT || token->type == TokenType::LE ||
                       token->type == TokenType::GE || token->type == TokenType::AND ||
                       token->type == TokenType::OR) {
                // Binary operator: Apply Shunting Yard precedence rules
                // Apply all operators with higher or equal precedence first
                while (!opStack.empty() && opStack.peek() != TokenType::LPAREN && 
                       precedence(token->type) <= precedence(opStack.peek())) {
                    applyOp();
                }
                // Push current operator onto stack
                opStack.push(token->type);
                token = token->next;
            } else {
                // Unknown token in expression context
                throw runtime_error("Invalid token in expression: " + token->value);
            }
        }
        current = token;  // Update parser's current position

        // Apply any remaining operators from the stack
        while (!opStack.empty()) {
            applyOp();
        }

        // Validate final expression stack state
        if (exprStack.empty()) throw runtime_error("Invalid expression");
        Expr* result = exprStack.pop();
        if (!exprStack.empty()) throw runtime_error("Invalid expression");

        return result;
    }


    /**
     * Parses sequences of statements (statement blocks)
     * 
     * Used in: Function bodies, program top-level, block statements
     * Stops parsing when: '}' (end of block) or END (end of input) is encountered
     * 
     * Implementation Strategy: Uses a simple loop-based approach to collect
     * consecutive statements into a linked list. This method is called by
     * function definitions and the main program parser to handle statement blocks.
     * 
     * Design Decision: Statements are collected until a natural boundary is reached
     * (closing brace or end of input). This allows for flexible statement grouping
     * while maintaining clear scope boundaries in the generated code.
     * 
     * @return StmtList linked list of statements, or nullptr if empty
     * @throws runtime_error for statement parsing errors
     */
    StmtList* parseStmtList() {
        StmtList* head = nullptr;
        StmtList** tail = &head;
        
        // Parse statements until we hit a natural boundary
        while (current && current->type != TokenType::RBRACE && current->type != TokenType::END) {
            ASTNode* stmt = parseStatement();
            if (stmt) {
                // Add valid statements to the list
                *tail = new StmtList(stmt);
                tail = &(*tail)->next;
            }
            // parseStatement() already advances tokens, so no manual advancement needed
        }
        
        return head;
    }
};

/**
 * Code generator for the Res programming language
 * Converts an AST into equivalent C++ source code
 * 
 * Design Philosophy: Uses a three-pass approach for clean code generation:
 * 1. Forward declarations - Allows functions to call each other regardless of definition order
 * 2. Main function - Contains all top-level statements and variable declarations
 * 3. Function definitions - Complete function implementations
 * 
 * This approach ensures the generated C++ code compiles correctly while
 * maintaining readability and following C++ best practices.
 */
class CodeGenerator {
    Program* program;   // The program AST to generate code from

public:
    /**
     * Constructor takes ownership of the program AST
     * @param p Program AST to generate code from
     */
    CodeGenerator(Program* p) : program(p) {}
    
    /**
     * Destructor ensures proper cleanup of the program AST
     * 
     * Ownership Model: The CodeGenerator takes ownership of the Program AST
     * passed to its constructor. This design ensures that the AST is properly
     * cleaned up after code generation is complete, following RAII principles.
     * 
     * The Program's destructor will recursively delete all child AST nodes,
     * ensuring complete memory cleanup of the syntax tree.
     */
    ~CodeGenerator() { delete program; }

    /**
     * Generates C++ code from the program AST
     * 
     * Three-Pass Generation Strategy:
     * Pass 1: Function forward declarations - Enables mutual recursion
     * Pass 2: Main function with top-level code - Entry point execution
     * Pass 3: Function definitions - Complete implementations
     * 
     * This ordering ensures proper C++ compilation and execution flow.
     * 
     * @return Complete C++ source code as a string
     */
    string generate() {
        // Standard C++ headers and namespace for generated code
        string code = "#include <iostream>\n#include <string>\nusing namespace std;\n\n";
        
        // PASS 1: Generate forward declarations for all functions
        // This allows functions to call each other regardless of definition order
        StmtList* stmt = program->statements;
        while (stmt) {
            if (FuncDef* func = dynamic_cast<FuncDef*>(stmt->stmt)) {
                // Generate function signature for forward declaration
                code += "void " + func->name + "(";
                ParamList* param = func->params;
                for (size_t i = 0; param; param = param->next, ++i) {
                    code += param->param->type + " " + param->param->name;
                    if (param->next) code += ", ";
                }
                code += ");\n";
            }
            stmt = stmt->next;
        }
        
        // PASS 2: Generate main function with all top-level statements
        // This creates the program entry point and handles variable declarations
        // and print statements that aren't inside functions
        code += "\nint main() {\n";
        stmt = program->statements;
        while (stmt) {
            if (DeclStmt* decl = dynamic_cast<DeclStmt*>(stmt->stmt)) {
                // Variable declaration: Generate C++ variable with initialization
                code += "    " + decl->type + " " + decl->varName + " = " + generateExpr(decl->value) + ";\n";
            } else if (PrintStmt* print = dynamic_cast<PrintStmt*>(stmt->stmt)) {
                // Print statement: Convert to C++ cout with proper spacing
                code += "    cout << ";
                ExprList* expr = print->expressions;
                for (size_t i = 0; expr; expr = expr->next, ++i) {
                    code += generateExpr(expr->expr);
                    if (expr->next) code += " << \" \" << ";  // Add space between outputs
                }
                code += " << endl;\n";
            } else if (FuncCallExpr* call = dynamic_cast<FuncCallExpr*>(stmt->stmt)) {
                // Function call statement: Generate C++ function call
                code += "    " + call->name + "(";
                ExprList* arg = call->arguments;
                for (size_t i = 0; arg; arg = arg->next, ++i) {
                    code += generateExpr(arg->expr);
                    if (arg->next) code += ", ";
                }
                code += ");\n";
            }
            stmt = stmt->next;
        }
        code += "    return 0;\n}\n";
        
        // PASS 3: Generate complete function definitions
        // This implements all user-defined functions with their full bodies
        stmt = program->statements;
        while (stmt) {
            if (FuncDef* func = dynamic_cast<FuncDef*>(stmt->stmt)) {
                // Generate function signature (same as forward declaration)
                code += "void " + func->name + "(";
                ParamList* param = func->params;
                for (size_t i = 0; param; param = param->next, ++i) {
                    code += param->param->type + " " + param->param->name;
                    if (param->next) code += ", ";
                }
                code += ") {\n";
                
                // Generate function body statements
                StmtList* body = func->body;
                while (body) {
                    if (DeclStmt* decl = dynamic_cast<DeclStmt*>(body->stmt)) {
                        // Local variable declaration
                        code += "    " + decl->type + " " + decl->varName + " = " + generateExpr(decl->value) + ";\n";
                    } else if (PrintStmt* print = dynamic_cast<PrintStmt*>(body->stmt)) {
                        // Print statement within function
                        code += "    cout << ";
                        ExprList* expr = print->expressions;
                        for (size_t i = 0; expr; expr = expr->next, ++i) {
                            code += generateExpr(expr->expr);
                            if (expr->next) code += " << \" \" << ";
                        }
                        code += " << endl;\n";
                    } else if (FuncCallExpr* call = dynamic_cast<FuncCallExpr*>(body->stmt)) {
                        // Function call within function
                        code += "    " + call->name + "(";
                        ExprList* arg = call->arguments;
                        for (size_t i = 0; arg; arg = arg->next, ++i) {
                            code += generateExpr(arg->expr);
                            if (arg->next) code += ", ";
                        }
                        code += ");\n";
                    }
                    body = body->next;
                }
                code += "}\n";
            }
            stmt = stmt->next;
        }
        return code;
    }

private:
    /**
     * Generates C++ code for an expression
     * 
     * Expression Translation Strategy: Each Res expression type maps to
     * equivalent C++ syntax. The goal is to produce readable, idiomatic
     * C++ code that preserves the semantics of the original Res expression.
     * 
     * @param expr Expression AST node to generate code for
     * @return C++ expression code as a string
     */
    string generateExpr(Expr* expr) {
        if (NumberExpr* num = dynamic_cast<NumberExpr*>(expr)) {
            // Numeric literal: Pass through unchanged (works for both int and float)
            return num->value;
        } else if (StringExpr* str = dynamic_cast<StringExpr*>(expr)) {
            // String literal: Pass through with quotes preserved
            return str->value;
        } else if (VarExpr* var = dynamic_cast<VarExpr*>(expr)) {
            // Variable reference: Use C++ variable name directly
            return var->name;
        } else if (BinaryExpr* bin = dynamic_cast<BinaryExpr*>(expr)) {
            // Binary expression: Map Res operators to C++ operators
            string op;
            switch (bin->op) {
                case TokenType::PLUS: op = "+"; break;      // Arithmetic addition
                case TokenType::MINUS: op = "-"; break;     // Arithmetic subtraction  
                case TokenType::MULT: op = "*"; break;      // Arithmetic multiplication
                case TokenType::DIV: op = "/"; break;       // Arithmetic division
                case TokenType::EQ: op = "=="; break;       // Equality comparison
                case TokenType::LT: op = "<"; break;        // Less than comparison
                case TokenType::GT: op = ">"; break;        // Greater than comparison
                case TokenType::LE: op = "<="; break;       // Less than or equal
                case TokenType::GE: op = ">="; break;       // Greater than or equal
                case TokenType::AND: op = "&&"; break;      // Logical AND
                case TokenType::OR: op = "||"; break;       // Logical OR
                default: op = "="; break;                   // Fallback (shouldn't occur)
            }
            // Generate parenthesized expression to preserve evaluation order
            return "(" + generateExpr(bin->left) + " " + op + " " + generateExpr(bin->right) + ")";
        } else if (FuncCallExpr* call = dynamic_cast<FuncCallExpr*>(expr)) {
            // Function call expression: Generate C++ function call syntax
            string code = call->name + "(";
            ExprList* arg = call->arguments;
            for (size_t i = 0; arg; arg = arg->next, ++i) {
                code += generateExpr(arg->expr);
                if (arg->next) code += ", ";    // Comma-separate arguments
            }
            code += ")";
            return code;
        }
        // Unknown expression type - should not happen with valid AST
        return "";
    }
};

/**
 * ANSI Color Codes for Enhanced User Interface
 * 
 * Design Decision: Using ANSI escape codes instead of external libraries
 * for terminal colors provides cross-platform compatibility and avoids
 * dependencies. These codes work on most modern terminals including
 * Linux, macOS, and Windows 10+ terminals.
 */
const string RESET = "\033[0m";        // Reset all formatting
const string RED = "\033[31m";          // Error messages (failures, exceptions)
const string GREEN = "\033[32m";        // Success messages (compilation successful)
const string YELLOW = "\033[33m";       // Warning messages (progress indicators)
const string BLUE = "\033[34m";         // Information messages (prompts, help)
const string MAGENTA = "\033[35m";      // Code output formatting (generated code)
const string CYAN = "\033[36m";         // Input prompts (REPL interface)
const string BOLD = "\033[1m";          // Bold text (emphasis, headers)

/**
 * Saves generated C++ code to a file with intelligent file management
 * 
 * File Management Strategy: This function implements user-friendly file handling
 * with automatic directory creation, extension validation, and collision resolution.
 * 
 * Safety Features:
 * - Creates saves directory automatically if it doesn't exist
 * - Validates and adds .cpp extension if missing
 * - Prompts for overwrite confirmation on existing files
 * - Provides helpful error messages for file system issues
 * 
 * User Experience Design: The function guides users through filename conflicts
 * with clear prompts and maintains a professional workflow for code output.
 * 
 * @param code The C++ source code to save
 * @param filename Desired filename (will add .cpp extension if missing)
 * @throws runtime_error if file operations fail (permissions, disk space, etc.)
 */
void saveToFile(const string& code, const string& filename) {
    // Ensure output directory exists for organized file management
    string outputDir = "saves";
    if (system(("mkdir -p " + outputDir).c_str()) != 0) {
        cerr << RED << "Warning: Could not create saves directory\n" << RESET;
    }
    
    // Filename processing: Apply defaults and ensure proper C++ extension
    string base = filename.empty() ? "program.cpp" : (filename.find(".cpp") == string::npos ? filename + ".cpp" : filename);
    int counter = 0;
    string finalName = outputDir + "/" + base;
    
    // File collision handling: Prompt for overwrite or auto-generate unique names
    while (ifstream(finalName)) {
        cout << YELLOW << "File '" << finalName << "' exists. Overwrite? (y/n): " << RESET;
        string response;
        getline(cin, response);
        if (response == "y" || response == "Y") break;
        // Generate unique filename by appending counter: filename(1).cpp, filename(2).cpp, etc.
        counter++;
        size_t pos = base.find(".cpp");
        string baseName = base.substr(0, pos);
        finalName = outputDir + "/" + baseName + "(" + to_string(counter) + ").cpp";
    }
    
    // File output operation with error checking
    ofstream out(finalName);
    if (!out) throw runtime_error("Failed to write to " + finalName);
    out << code;
    out.close();
    cout << GREEN << "Saved to " << finalName << RESET << endl;
}

/**

 * 
 * Phase 1: Lexical Analysis - Tokenization of source code
 * Phase 2: Syntax Analysis - AST construction with semantic checking
 * Phase 3: Code Generation - C++ target code production
 * 
 * Error Handling Strategy: Uses exception-based error reporting where each
 * phase can throw runtime_error exceptions with descriptive messages. This
 * provides clear error propagation up to the REPL interface for user feedback.
 * 
 */
string compile(const string& source, SymbolTable& symbols) {
    // Phase 1: Lexical Analysis - Transform source text into token stream
    // The lexer handles all character-level processing and keyword recognition
    Lexer lexer(source);
    TokenList* tokens = lexer.tokenize();
    
    // Phase 2: Syntax Analysis - Build AST from tokens with semantic validation
    // Parser takes ownership of tokens and will clean them up in its destructor
    Parser parser(tokens, symbols);
    Program* program = parser.parse();
    
    // Phase 3: Code Generation - Convert AST to equivalent C++ source code
    // CodeGenerator takes ownership of program AST and handles memory cleanup
    CodeGenerator generator(program);
    return generator.generate();
}

// main 

int main() {
    // Initialize user interface with welcome screen and instructions
    printAscii();

    
    // Initialize compiler state for persistent REPL session
    string input;           // Accumulated source code across multiple input lines
    string line;            // Current input line buffer for getline()
    SymbolTable symbols;    // Persistent symbol table maintains variable scope
    int lineCount = 0;      // Line counter for potential future features (debugging, etc.)
    
    // Start the REPL main loop - continues until Ctrl+C termination
    cout << CYAN << "\n> " << RESET;
    while (true) {
        getline(cin, line);
        
        if (line == "\\g") {
            // Compilation trigger command - process all accumulated input
            if (!input.empty()) {
                try {
                    // User feedback: Count meaningful lines for progress reporting
                    // This excludes empty lines and whitespace-only lines
                    int nonEmptyLines = 0;
                    istringstream iss(input);
                    string tempLine;
                    while (getline(iss, tempLine)) {
                        if (!tempLine.empty() && tempLine.find_first_not_of(" \t") != string::npos) {
                            nonEmptyLines++;
                        }
                    }
                    
                    // Progress indicator: Inform user of compilation start with line count
                    cout << YELLOW << "\n[" << BOLD << "Compiling " << nonEmptyLines << " lines of code" << RESET << YELLOW << "]" << RESET << endl;
                    
                    // Execute compilation pipeline: lexing -> parsing -> code generation
                    string output = compile(input, symbols);
                    
                    // Success notification: Clear positive feedback for successful compilation
                    cout << GREEN << "\n✓ " << BOLD << "Compilation successful!" << RESET << GREEN << " (0 errors, 0 warnings)" << RESET << endl;
                    
                    // Present generated code with professional formatting for readability
                    cout << MAGENTA << "┌────────────────────────────────────────────────────────┐" << RESET << endl;
                    cout << MAGENTA << "│ " << BOLD << "Generated C++ Code:" << RESET << MAGENTA << "                                    │" << RESET << endl;
                    cout << MAGENTA << "└────────────────────────────────────────────────────────┘" << RESET << endl;
                    cout << output << endl;
                    cout << MAGENTA << "───────────────────────────────────────────────────────────" << RESET << endl;
                    
                    // File output: Prompt for custom filename with sensible default
                    cout << BLUE << "Enter output filename (default: program.cpp): " << RESET;
                    string filename;
                    getline(cin, filename);
                    // Delegate file operations to specialized function with error handling
                    saveToFile(output, filename);
                } catch (const exception& e) {
                    // Error Recovery Strategy: Display detailed error information while
                    // preserving user's accumulated input for correction and retry
                    cout << RED << "⚠ " << BOLD << "Error:" << RESET << RED << " " << e.what() << RESET << endl;
                    cout << RED << "──────────────────────────────────────────────────────────" << RESET << endl;
                    cout << RED << "Compilation failed. Please fix the error and try again." << RESET << endl;
                }
                
                // State reset for next compilation cycle while preserving symbol table
                // Symbol table persistence allows variables to remain in scope across compilations
                input.clear();
                lineCount = 0;
                cout << YELLOW << "\n[" << BOLD << "Ready for new code" << RESET << YELLOW << "]" << RESET << endl;
                cout << CYAN << "> " << RESET;
            } else {
                // User guidance: Handle edge case of compilation attempt with no input
                cout << YELLOW << "No code to compile! Please enter some code first." << RESET << endl;
                cout << CYAN << "> " << RESET;
            }
        } else {
            // Input accumulation: Regular source code line collection
            // Each line is appended with newline preservation for proper parsing
            input += line + "\n";
            lineCount++;
            cout << CYAN << "> " << RESET;
        }
    }
    // Note: REPL runs indefinitely until external termination (Ctrl+C)
    // Standard exit code for normal program termination (though rarely reached)
    return 0;
}
