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
 * 4. "Hierarchical Tree Structure Nodes": The structures that represent your code's meaning.
 * 5. "Syntax Analysis (Parser)": How the tokens are organized into a meaningful structure (the hierarchical tree).
 * 6. "Main Program & REPL": Where the compiler starts and how it can be used interactively.
 *
 * --- 1. Core Building Blocks (Search: "enum class TokenType", "struct Token", "struct TokenQueue") ---
 *    - `enum class TokenType`: Lists all categories of code elements (e.g., keywords like 'typeshi', operators like '+').
 *    - `struct Token`: Represents a single piece of code, like a word or symbol, with its type and actual text.
 *    - `struct TokenQueue`: A queue that holds all the Tokens in FIFO order as they appear in the code.
 *
 * --- 2. Lexical Analysis (Lexer) (Search: "class Lexer") ---
 *    - `class Lexer`: This part reads your Res source code character by character.
 *      Its main job (`tokenize` method) is to convert the text into a sequence of Tokens (a `TokenQueue`).
 *      It identifies things like keywords, variable names (`parseIdentifier`), numbers (`parseNumber`),
 *      strings (`parseStringLiteral`), and comments (`parseComment`).
 *
 * --- 3. Symbol Table (Search: "class SymbolTable", "struct BSTNode") ---
 *    - `class SymbolTable`: Think of this as the compiler's memory for variables.
 *      It uses a Binary Search Tree (`BSTNode`) to keep track of all declared variables,
 *      allowing quick checks (`isDeclared`) and additions (`addVariable`).
 *
 * --- 4. Hierarchical Tree Structure Nodes (Search: "struct TreeNode", "struct ExprNode", "struct DeclStmt") ---
 *    - `struct TreeNode`: The basic building block for the tree structure. All other node types are derived from this.
 *    - Expression Nodes (e.g., `NumberNode`, `VarNode`, `BinaryNode`): Represent parts of code that produce values,
 *      like numbers, variable uses, or calculations (e.g., `a + b`).
 *    - Statement Nodes (e.g., `DeclStmt`, `PrintStmt`, `FuncDef`): Represent actions or declarations,
 *      like creating a variable (`typeshi int x = 10;`), printing something (`coms(x);`), or defining a function.
 *    - `struct Program`: The root of the tree structure, holding all statements of your Res program.
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
 *       - `TokenQueue`: Stores the sequence of `Token`s generated by the `Lexer`. It's a FIFO queue structure.
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
 *     - These tokens are stored in a `TokenQueue`.
 *  b. Syntax Analysis (Parsing):
 *     - The `Parser` class takes the `TokenQueue` from the Lexer.
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
 * Queue implementation for token management using FIFO (First-In-First-Out) ordering
 * Provides efficient enqueue/dequeue operations for token processing
 */
struct TokenQueue {
    Token* front;       // Front of the queue (for dequeuing)
    Token* rear;        // Rear of the queue (for enqueuing)
    
    /**
     * Constructor initializes empty queue
     */
    TokenQueue() : front(nullptr), rear(nullptr) {}
    
    /**
     * Destructor implements comprehensive cleanup of all token resources
     * 
     * Memory Management Strategy: Traverses the entire queue and deallocates
     * each Token object. This prevents memory leaks in the lexical analysis
     * phase of compilation.
     * 
     * Implementation Pattern: Uses iterative traversal with temporary pointer
     * management to safely delete each node while advancing through the queue.
     */
    ~TokenQueue() {
        Token* current = front;
        while (current) {
            Token* next = current->next;
            delete current;
            current = next;
        }
    }
    
    /**
     * Enqueues a token to the rear of the queue (FIFO order)
     * @param token Token to enqueue
     */
    void enqueue(Token* token) {
        if (!front) {
            front = rear = token;
        } else {
            rear->next = token;
            rear = token;
        }
    }
    
    /**
     * Dequeues a token from the front of the queue (FIFO order)
     * @return Token from front of queue, nullptr if empty
     */
    Token* dequeue() {
        if (!front) return nullptr;
        Token* token = front;
        front = front->next;
        if (!front) rear = nullptr; // Queue is now empty
        token->next = nullptr; // Disconnect from queue
        return token;
    }
    
    /**
     * Peeks at the front token without removing it
     * @return Token at front of queue, nullptr if empty
     */
    Token* peek() const {
        return front;
    }
    
    /**
     * Checks if the queue is empty
     * @return true if queue is empty, false otherwise
     */
    bool isEmpty() const {
        return front == nullptr;
    }
};

/**
 * Lexical analyzer for the Res programming language
 * Converts source code text into a stream of tokens using Queue data structure
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
     * Tokenizes the entire input string using Queue for FIFO token processing
     * @return TokenQueue containing all tokens found in input
     */
    TokenQueue* tokenize() {
        TokenQueue* tokens = new TokenQueue();
        while (pos < input.length()) {
            char c = input[pos];
            if (isspace(c)) {
                // Handle whitespace but preserve newlines as they are important for statement separation
                if (c == '\n') {
                    // We could add a special NEWLINE token here if needed
                    // tokens->enqueue(new Token(TokenType::NEWLINE, "\\n"));
                }
                pos++;
                continue;
            }
            if (isalpha(c)) {
                tokens->enqueue(parseIdentifier());
            } else if (isdigit(c) || c == '.') {
                tokens->enqueue(parseNumber());
            } else if (c == '"') {
                tokens->enqueue(parseStringLiteral());
            } else if (c == '=') {
                if (pos + 1 < input.length() && input[pos + 1] == '=') {
                    tokens->enqueue(new Token(TokenType::EQ, "=="));
                    pos += 2;
                } else if (pos + 1 < input.length() && input[pos + 1] == '>') {
                    tokens->enqueue(new Token(TokenType::ARROW, "=>"));
                    pos += 2;
                } else {
                    tokens->enqueue(new Token(TokenType::EQUALS, "="));
                    pos++;
                }
            } else if (c == ';') {
                tokens->enqueue(new Token(TokenType::SEMICOLON, ";"));
                pos++;
            } else if (c == '(') {
                tokens->enqueue(new Token(TokenType::LPAREN, "("));
                pos++;
            } else if (c == ')') {
                tokens->enqueue(new Token(TokenType::RPAREN, ")"));
                pos++;
            } else if (c == '{') {
                tokens->enqueue(new Token(TokenType::LBRACE, "{"));
                pos++;
            } else if (c == '}') {
                tokens->enqueue(new Token(TokenType::RBRACE, "}"));
                pos++;
            } else if (c == ',') {
                tokens->enqueue(new Token(TokenType::COMMA, ","));
                pos++;
            } else if (c == '+') {
                tokens->enqueue(new Token(TokenType::PLUS, "+"));
                pos++;
            } else if (c == '-') {
                tokens->enqueue(new Token(TokenType::MINUS, "-"));
                pos++;
            } else if (c == '*') {
                tokens->enqueue(new Token(TokenType::MULT, "*"));
                pos++;
            } else if (c == '/') {
                if (pos + 1 < input.length() && input[pos + 1] == '/') {
                    tokens->enqueue(parseComment());
                } else {
                    tokens->enqueue(new Token(TokenType::DIV, "/"));
                    pos++;
                }
            } else if (c == '<') {
                if (pos + 1 < input.length() && input[pos + 1] == '=') {
                    tokens->enqueue(new Token(TokenType::LE, "<="));
                    pos += 2;
                } else {
                    tokens->enqueue(new Token(TokenType::LT, "<"));
                    pos++;
                }
            } else if (c == '>') {
                if (pos + 1 < input.length() && input[pos + 1] == '=') {
                    tokens->enqueue(new Token(TokenType::GE, ">="));
                    pos += 2;
                } else {
                    tokens->enqueue(new Token(TokenType::GT, ">"));
                    pos++;
                }
            } else if (c == '&') {
                if (pos + 1 < input.length() && input[pos + 1] == '&') {
                    tokens->enqueue(new Token(TokenType::AND, "&&"));
                    pos += 2;
                } else {
                    throw runtime_error("Invalid character: &");
                }
            } else if (c == '|') {
                if (pos + 1 < input.length() && input[pos + 1] == '|') {
                    tokens->enqueue(new Token(TokenType::OR, "||"));
                    pos += 2;
                } else {
                    throw runtime_error("Invalid character: |");
                }
            } else {
                throw runtime_error("Unknown character: " + string(1, c));
            }
        }
        tokens->enqueue(new Token(TokenType::END, ""));
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


class SymbolTable {

    BSTNode* root;      // Root of the BST
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

    bool find(BSTNode* node, const string& var) const {
        if (!node) return false;                    // Base case: not found
        if (var == node->value) return true;        // Base case: found
        if (var < node->value) return find(node->left, var);   // Search left
        return find(node->right, var);              // Search right
    }

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

    ~SymbolTable() { clear(root); }


    void addVariable(const string& var) {
        insert(root, var);
    }

    bool isDeclared(const string& var) const {
        return find(root, var);
    }
};

struct TreeNode {
    virtual ~TreeNode() = default;
};


struct ExprNode : TreeNode {
    virtual ~ExprNode() = default;
};


struct NumberNode : ExprNode {
    string value;       // The numeric value as a string
    NumberNode(const string& v) : value(v) {}
};


struct StringNode : ExprNode {
    string value;       // The string value including quotes
    StringNode(const string& v) : value(v) {}
};

struct VarNode : ExprNode {
    string name;        // The variable name
    VarNode(const string& n) : name(n) {}
};


struct BinaryNode : ExprNode {
    ExprNode* left;     // Left operand
    TokenType op;       // Operator type
    ExprNode* right;    // Right operand
    
    BinaryNode() : left(nullptr), right(nullptr) {}
    ~BinaryNode() { delete left; delete right; }
};


struct FuncCallNode : ExprNode {
    string name;                    // Function name
    struct ExprList* arguments;     // List of arguments
    
    FuncCallNode() : arguments(nullptr) {}
    ~FuncCallNode();    // Forward declaration, defined later
};


struct ExprList {
    ExprNode* expr;         // Current expression
    ExprList* next;     // Next expression in list
    
    ExprList(ExprNode* e) : expr(e), next(nullptr) {}
    ~ExprList() { delete expr; delete next; }
};


struct DeclStmt : TreeNode {
    string type;        // Variable type (int, float, string)
    string varName;     // Variable name
    ExprNode* value;        // Initial value expression
    
    DeclStmt() : value(nullptr) {}
    ~DeclStmt() { delete value; }
};


struct PrintStmt : TreeNode {
    ExprList* expressions;  // List of expressions to print
    
    PrintStmt() : expressions(nullptr) {}
    ~PrintStmt() { delete expressions; }
};


struct FuncDef : TreeNode {
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
    TreeNode* stmt;      // Current statement
    StmtList* next;     // Next statement in list
    
    StmtList(TreeNode* s) : stmt(s), next(nullptr) {}
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
 * Root tree node representing a complete program
 */
struct Program : TreeNode {
    StmtList* statements;   // All statements in the program
    
    Program() : statements(nullptr) {}

    ~Program() { delete statements; }
};


FuncCallNode::~FuncCallNode() { delete arguments; }    // Cleanup argument expression list
FuncDef::~FuncDef() { delete params; delete body; }    // Cleanup parameters and function body


struct ParseState {
    string state;       // Current parsing state description
    TreeNode* node;      // Associated tree node being built
    
    ParseState(string s, TreeNode* n) : state(s), node(n) {}
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
     * @param node Associated tree node
     */
    void push(string state, TreeNode* node) {
        StackNode* newNode = new StackNode(new ParseState(state, node));
        newNode->next = top;
        top = newNode;
    }
    
    /**
     * Pops the top parsing state from the stack
     * return ParseState from top of stack
     * throws runtime_error if stack is empty
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
     * return true if stack is empty, false otherwise
     */
    bool empty() const { return top == nullptr; }
};

/**
 * Syntax analyzer for the Res programming language
 * Converts tokens into a Hierarchical Tree Structure using Queue-based token processing
 * Uses a stack-based parsing approach for better control flow
 */
class Parser {
    TokenQueue* tokens;     // Input token queue for FIFO processing
    Token* current;         // Current token being processed
    ParseStack stack;       // Stack for managing parsing states
    SymbolTable& symbols;   // Symbol table for variable tracking

public:

    Parser(TokenQueue* t, SymbolTable& s) : tokens(t), current(t->peek()), symbols(s) {}
    
    ~Parser() { delete tokens; }

    Program* parse() {
        Program* program = new Program();
        StmtList** tail = &program->statements;
        
        // Parse all statements until end of input using queue-based token processing
        while (current && current->type != TokenType::END) {
            TreeNode* stmt = parseStatement();
            if (stmt) {
                *tail = new StmtList(stmt);
                tail = &(*tail)->next;
            }
            // parseStatement handles token advancement
        }
        return program;
    }

private:
 
    TreeNode* parseStatement() {
        if (!current) return nullptr;
        
        // Skip any whitespace tokens
        while (current && isspace(current->value[0])) {
            current = current->next;
        }
        
        if (!current) return nullptr;
        
        TreeNode* result = nullptr;
        
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

    DeclStmt* parseDeclStmt() {
        stack.push("expect_typeshi", nullptr);
        DeclStmt* decl = nullptr;
        while (!stack.empty()) {
            ParseState* state = stack.pop();
            string s = state->state;
            TreeNode* node = state->node;
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

    PrintStmt* parsePrintStmt() {
        stack.push("expect_coms", nullptr);
        PrintStmt* print = nullptr;
        while (!stack.empty()) {
            ParseState* state = stack.pop();
            string s = state->state;
            TreeNode* node = state->node;
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

    FuncDef* parseFuncDef() {
        stack.push("expect_func", nullptr);
        FuncDef* func = nullptr;
        while (!stack.empty()) {
            ParseState* state = stack.pop();
            string s = state->state;
            TreeNode* node = state->node;
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

    FuncCallNode* parseFuncCallStmt() {
        stack.push("expect_identifier", nullptr);
        FuncCallNode* call = nullptr;
        while (!stack.empty()) {
            ParseState* state = stack.pop();
            string s = state->state;
            TreeNode* node = state->node;
            delete state;
            if (s == "expect_identifier") {
                // State 1: Expect function name identifier
                if (current->type != TokenType::IDENTIFIER) throw runtime_error("Expected identifier");
                call = new FuncCallNode();
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
                call = dynamic_cast<FuncCallNode*>(node);
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

    ExprNode* parseExpression() {
        // Use proper Stack data structures
        class ExprStack {
            struct Node {
                ExprNode* data;
                Node* next;
                Node(ExprNode* d) : data(d), next(nullptr) {}
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
                    delete temp->data;  // Clean up Expression tree node
                    delete temp;        // Clean up stack node structure
                }
            }
            void push(ExprNode* expr) {
                Node* newNode = new Node(expr);
                newNode->next = top;
                top = newNode;
            }
            ExprNode* pop() {
                if (!top) throw runtime_error("Expression stack underflow");
                Node* temp = top;
                top = top->next;
                ExprNode* data = temp->data;
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
            ExprNode* right = exprStack.pop();  // Second operand (right side)
            ExprNode* left = exprStack.pop();   // First operand (left side)
            
            // Create binary expression tree node
            BinaryNode* bin = new BinaryNode();
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
                exprStack.push(new NumberNode(token->value));
                token = token->next;
            } else if (token->type == TokenType::STRING_LITERAL) {
                // Operand: String literals are pushed directly onto expression stack
                exprStack.push(new StringNode(token->value));
                token = token->next;
            } else if (token->type == TokenType::IDENTIFIER) {
                // Operand: Variable references (with scope checking)
                if (!symbols.isDeclared(token->value)) 
                    throw runtime_error("Undefined variable: " + token->value);
                exprStack.push(new VarNode(token->value));
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
        ExprNode* result = exprStack.pop();
        if (!exprStack.empty()) throw runtime_error("Invalid expression");

        return result;
    }

    StmtList* parseStmtList() {
        StmtList* head = nullptr;
        StmtList** tail = &head;
        
        // Parse statements until we hit a natural boundary
        while (current && current->type != TokenType::RBRACE && current->type != TokenType::END) {
            TreeNode* stmt = parseStatement();
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

/*
 * Code generator for the Res programming language
 * Converts a hierarchical tree structure into equivalent C++ source code
 */
class CodeGenerator {
    Program* program;   // The program tree to generate code from

public:

    CodeGenerator(Program* p) : program(p) {}
    
    ~CodeGenerator() { delete program; }

 
    string generate() {
        // Standard C++ headers and namespace for generated code
        string code = "#include <iostream>\n#include <string>\nusing namespace std;\n\n";
        
        // PASS 1: Generate forward declarations for all functions
        // This allows functions to call each other regardless of definition order
        StmtList* stmt = program->statements;
        while (stmt) {
            // if have sya function
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
            } else if (FuncCallNode* call = dynamic_cast<FuncCallNode*>(stmt->stmt)) {
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
                    } else if (FuncCallNode* call = dynamic_cast<FuncCallNode*>(body->stmt)) {
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

    string generateExpr(ExprNode* expr) {
        if (NumberNode* num = dynamic_cast<NumberNode*>(expr)) {
            // Numeric literal: Pass through unchanged (works for both int and float)
            return num->value;
        } else if (StringNode* str = dynamic_cast<StringNode*>(expr)) {
            // String literal: Pass through with quotes preserved
            return str->value;
        } else if (VarNode* var = dynamic_cast<VarNode*>(expr)) {
            // Variable reference: Use C++ variable name directly
            return var->name;
        } else if (BinaryNode* bin = dynamic_cast<BinaryNode*>(expr)) {
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
        } else if (FuncCallNode* call = dynamic_cast<FuncCallNode*>(expr)) {
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
        // Unknown expression type - should not happen with valid hierarchical tree
        return "";
    }
};

const string RESET = "\033[0m";        // Reset all formatting
const string RED = "\033[31m";          // Error messages (failures, exceptions)
const string GREEN = "\033[32m";        // Success messages (compilation successful)
const string YELLOW = "\033[33m";       // Warning messages (progress indicators)
const string BLUE = "\033[34m";         // Information messages (prompts, help)
const string MAGENTA = "\033[35m";      // Code output formatting (generated code)
const string CYAN = "\033[36m";         // Input prompts (REPL interface)
const string BOLD = "\033[1m";          // Bold text (emphasis, headers)

// Save the generated code into the save directory
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

// This is where the magic happens
string compile(const string& source, SymbolTable& symbols) {
    // #2 Lexer: Phase 1: Lexical Analysis - Transform source text into token stream
    // The lexer handles all character-level processing and keyword recognition
    Lexer lexer(source);
    // #3 TokenQueue: FIFO queue storing tokens in order for sequential processing
    TokenQueue* tokens = lexer.tokenize();
    
    // #4 Parser: Phase 2: Syntax Analysis - Build hierarchical tree from tokens with semantic validation
    // Parser takes ownership of tokens and will clean them up in its destructor
    Parser parser(tokens, symbols);
    // #5 Hierarchical Tree: Tree structure representing program structure (Program, statements, expressions, functions)
    Program* program = parser.parse();
    
    // #6 CodeGenerator: Phase 3: Code Generation - Convert hierarchical tree to equivalent C++ source code
    // CodeGenerator takes ownership of program tree and handles memory cleanup
    CodeGenerator generator(program);
    // #7 C++ String: Complete generated C++ source code ready for compilation
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
    // #1
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
                    
                    // #7 C++ String: Execute compilation pipeline: lexing -> parsing -> code generation
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
                    // #8 File Save: Delegate file operations to specialized function with error handling
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
