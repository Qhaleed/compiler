#include <iostream>
#include <string>
#include <fstream>
#include <cctype>
#include <sstream>

using namespace std;

// Token types
enum class TokenType {
    TYPESHI, INT, FLOAT, STRING, COMS, FUNC, IDENTIFIER, NUMBER, STRING_LITERAL,
    EQUALS, SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE, COMMA, PLUS, MINUS,
    MULT, DIV, EQ, LT, GT, LE, GE, AND, OR, ARROW, COMMENT, END
};

// Token structure
struct Token {
    TokenType type;
    string value;
    Token* next; // For linked list
    Token(TokenType t, string v) : type(t), value(v), next(nullptr) {}
};

// Custom List for tokens
struct TokenList {
    Token* head;
    Token* tail;
    TokenList() : head(nullptr), tail(nullptr) {}
    ~TokenList() {
        Token* current = head;
        while (current) {
            Token* next = current->next;
            delete current;
            current = next;
        }
    }
    void append(Token* token) {
        if (!head) {
            head = tail = token;
        } else {
            tail->next = token;
            tail = token;
        }
    }
};

// Lexer class
class Lexer {
    string input;
    size_t pos;

public:
    Lexer(const string& src) : input(src), pos(0) {}

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
    Token* parseIdentifier() {
        string value;
        while (pos < input.length() && isalnum(input[pos])) {
            value += input[pos++];
        }
        if (value == "typeshi") return new Token(TokenType::TYPESHI, value);
        if (value == "int") return new Token(TokenType::INT, value);
        if (value == "float") return new Token(TokenType::FLOAT, value);
        if (value == "string") return new Token(TokenType::STRING, value);
        if (value == "coms") return new Token(TokenType::COMS, value);
        if (value == "func") return new Token(TokenType::FUNC, value);
        return new Token(TokenType::IDENTIFIER, value);
    }

    Token* parseNumber() {
        string value;
        // Just collect all digits and dots without additional logic for now
        while (pos < input.length() && (isdigit(input[pos]) || input[pos] == '.')) {
            value += input[pos++];
        }
        return new Token(TokenType::NUMBER, value);
    }

    Token* parseStringLiteral() {
        string value;
        pos++; // Skip opening quote
        while (pos < input.length() && input[pos] != '"') {
            value += input[pos++];
        }
        if (pos >= input.length()) throw runtime_error("Unterminated string literal");
        pos++; // Skip closing quote
        return new Token(TokenType::STRING_LITERAL, "\"" + value + "\"");
    }

    Token* parseComment() {
        string value = "//";
        pos += 2; // Skip //
        while (pos < input.length() && input[pos] != '\n') {
            value += input[pos++];
        }
        // Also consume the newline character if present
        if (pos < input.length() && input[pos] == '\n') {
            pos++;
        }
        return new Token(TokenType::COMMENT, value);
    }
};

// Custom BST for symbol table
struct BSTNode {
    string value;
    BSTNode* left;
    BSTNode* right;
    BSTNode(string v) : value(v), left(nullptr), right(nullptr) {}
};

class SymbolTable {
    BSTNode* root;

    void insert(BSTNode*& node, const string& var) {
        if (!node) {
            node = new BSTNode(var);
            return;
        }
        if (var < node->value) insert(node->left, var);
        else if (var > node->value) insert(node->right, var);
    }

    bool find(BSTNode* node, const string& var) const {
        if (!node) return false;
        if (var == node->value) return true;
        if (var < node->value) return find(node->left, var);
        return find(node->right, var);
    }

    void clear(BSTNode* node) {
        if (!node) return;
        clear(node->left);
        clear(node->right);
        delete node;
    }

public:
    SymbolTable() : root(nullptr) {}
    ~SymbolTable() { clear(root); }

    void addVariable(const string& var) {
        insert(root, var);
    }

    bool isDeclared(const string& var) const {
        return find(root, var);
    }
};

// AST nodes
struct ASTNode {
    virtual ~ASTNode() = default;
};

struct Expr : ASTNode {
    virtual ~Expr() = default;
};

struct NumberExpr : Expr {
    string value;
    NumberExpr(const string& v) : value(v) {}
};

struct StringExpr : Expr {
    string value;
    StringExpr(const string& v) : value(v) {}
};

struct VarExpr : Expr {
    string name;
    VarExpr(const string& n) : name(n) {}
};

struct BinaryExpr : Expr {
    Expr* left;
    TokenType op;
    Expr* right;
    BinaryExpr() : left(nullptr), right(nullptr) {}
    ~BinaryExpr() { delete left; delete right; }
};

struct FuncCallExpr : Expr {
    string name;
    struct ExprList* arguments;
    FuncCallExpr() : arguments(nullptr) {}
    ~FuncCallExpr();
};

struct ExprList {
    Expr* expr;
    ExprList* next;
    ExprList(Expr* e) : expr(e), next(nullptr) {}
    ~ExprList() { delete expr; delete next; }
};

struct DeclStmt : ASTNode {
    string type;
    string varName;
    Expr* value;
    DeclStmt() : value(nullptr) {}
    ~DeclStmt() { delete value; }
};

struct PrintStmt : ASTNode {
    ExprList* expressions;
    PrintStmt() : expressions(nullptr) {}
    ~PrintStmt() { delete expressions; }
};

struct FuncDef : ASTNode {
    string name;
    struct ParamList* params;
    struct StmtList* body;
    FuncDef() : params(nullptr), body(nullptr) {}
    ~FuncDef();
};

struct Param {
    string type;
    string name;
    Param* next;
    Param(string t, string n) : type(t), name(n), next(nullptr) {}
    ~Param() { delete next; }
};

struct ParamList {
    Param* param;
    ParamList* next;
    ParamList(Param* p) : param(p), next(nullptr) {}
    ~ParamList() { delete param; delete next; }
};

struct StmtList {
    ASTNode* stmt;
    StmtList* next;
    StmtList(ASTNode* s) : stmt(s), next(nullptr) {}
    ~StmtList() { delete stmt; delete next; }
};

struct Program : ASTNode {
    StmtList* statements;
    Program() : statements(nullptr) {}
    ~Program() { delete statements; }
};

// Forward declarations for destructors
FuncCallExpr::~FuncCallExpr() { delete arguments; }
FuncDef::~FuncDef() { delete params; delete body; }

// Custom Stack for parsing
struct ParseState {
    string state;
    ASTNode* node;
    ParseState(string s, ASTNode* n) : state(s), node(n) {}
};

// Stack datastructure
struct StackNode {
    ParseState* data;
    StackNode* next;
    StackNode(ParseState* d) : data(d), next(nullptr) {}
};

class ParseStack {
    StackNode* top;
public:
    ParseStack() : top(nullptr) {}
    ~ParseStack() {
        while (top) {
            StackNode* temp = top;
            top = top->next;
            delete temp->data;
            delete temp;
        }
    }

    // Stack pushing 
    void push(string state, ASTNode* node) {
        StackNode* newNode = new StackNode(new ParseState(state, node));
        newNode->next = top;
        top = newNode;
    }
    // Stack poping
    ParseState* pop() {
        if (!top) throw runtime_error("Stack underflow");
        StackNode* temp = top;
        top = top->next;
        ParseState* data = temp->data;
        delete temp;
        return data;
    }
    bool empty() const { return top == nullptr; }
};

// Parser class
class Parser {
    TokenList* tokens;
    Token* current;
    ParseStack stack;
    SymbolTable& symbols;

public:
    Parser(TokenList* t, SymbolTable& s) : tokens(t), current(t->head), symbols(s) {}
    ~Parser() { delete tokens; }

    Program* parse() {
        Program* program = new Program();
        StmtList** tail = &program->statements;
        while (current && current->type != TokenType::END) {
            ASTNode* stmt = parseStatement();
            if (stmt) {
                *tail = new StmtList(stmt);
                tail = &(*tail)->next;
            }
            // Don't automatically advance to the next token here
            // Let parseStatement handle token advancement
        }
        return program;
    }

private:
    ASTNode* parseStatement() {
        if (!current) return nullptr;
        
        // Skip any whitespace or newline tokens
        while (current && isspace(current->value[0])) {
            current = current->next;
        }
        
        if (!current) return nullptr;
        
        ASTNode* result = nullptr;
        if (current->type == TokenType::TYPESHI) {
            result = parseDeclStmt();
            return result; // Don't double-advance
        } else if (current->type == TokenType::COMS) {
            result = parsePrintStmt();
            return result; // Don't double-advance
        } else if (current->type == TokenType::FUNC) {
            result = parseFuncDef();
            return result; // Don't double-advance
        } else if (current->type == TokenType::IDENTIFIER) {
            result = parseFuncCallStmt();
            return result; // Don't double-advance
        } else if (current->type == TokenType::COMMENT) {
            // Skip comments
            current = current->next;
            return nullptr;
        } else if (current->type == TokenType::END) {
            // End of input
            return nullptr;
        } else {
            string invalidToken = current->value;
            current = current->next; // Skip unrecognized tokens
            throw runtime_error("Unexpected token: " + invalidToken);
        }
    }

    DeclStmt* parseDeclStmt() {
        stack.push("expect_typeshi", nullptr);
        DeclStmt* decl = nullptr;
        while (!stack.empty()) {
            ParseState* state = stack.pop();
            string s = state->state;
            ASTNode* node = state->node;
            delete state;
            if (s == "expect_typeshi") {
                if (current->type != TokenType::TYPESHI) throw runtime_error("Expected typeshi");
                current = current->next;
                stack.push("expect_type", nullptr);
            } else if (s == "expect_type") {
                if (current->type != TokenType::INT && current->type != TokenType::FLOAT && current->type != TokenType::STRING) {
                    throw runtime_error("Expected int, float, or string");
                }
                decl = new DeclStmt();
                decl->type = current->value;
                current = current->next;
                stack.push("expect_identifier", decl);
            } else if (s == "expect_identifier") {
                if (current->type != TokenType::IDENTIFIER) throw runtime_error("Expected identifier");
                decl = dynamic_cast<DeclStmt*>(node);
                decl->varName = current->value;
                symbols.addVariable(decl->varName);
                current = current->next;
                stack.push("expect_equals", decl);
            } else if (s == "expect_equals") {
                if (current->type != TokenType::EQUALS) throw runtime_error("Expected =");
                current = current->next;
                stack.push("expect_expression", node);
            } else if (s == "expect_expression") {
                decl = dynamic_cast<DeclStmt*>(node);
                decl->value = parseExpression();
                stack.push("expect_semicolon", decl);
            } else if (s == "expect_semicolon") {
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
            ASTNode* node = state->node;
            delete state;
            if (s == "expect_coms") {
                if (current->type != TokenType::COMS) throw runtime_error("Expected coms");
                current = current->next;
                stack.push("expect_lparen", nullptr);
            } else if (s == "expect_lparen") {
                if (current->type != TokenType::LPAREN) throw runtime_error("Expected (");
                current = current->next;
                print = new PrintStmt();
                stack.push("expect_expr_list", print);
            } else if (s == "expect_expr_list") {
                print = dynamic_cast<PrintStmt*>(node);
                print->expressions = parseExprList();
                stack.push("expect_rparen", print);
            } else if (s == "expect_rparen") {
                if (current->type != TokenType::RPAREN) throw runtime_error("Expected )");
                current = current->next;
                stack.push("expect_semicolon", node);
            } else if (s == "expect_semicolon") {
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
            ASTNode* node = state->node;
            delete state;
            if (s == "expect_func") {
                if (current->type != TokenType::FUNC) throw runtime_error("Expected func");
                current = current->next;
                stack.push("expect_identifier", nullptr);
            } else if (s == "expect_identifier") {
                if (current->type != TokenType::IDENTIFIER) throw runtime_error("Expected identifier");
                func = new FuncDef();
                func->name = current->value;
                current = current->next;
                stack.push("expect_lparen", func);
            } else if (s == "expect_lparen") {
                if (current->type != TokenType::LPAREN) throw runtime_error("Expected (");
                current = current->next;
                stack.push("expect_param_list", node);
            } else if (s == "expect_param_list") {
                func = dynamic_cast<FuncDef*>(node);
                func->params = parseParamList();
                stack.push("expect_rparen", node);
            } else if (s == "expect_rparen") {
                if (current->type != TokenType::RPAREN) throw runtime_error("Expected )");
                current = current->next;
                stack.push("expect_arrow", node);
            } else if (s == "expect_arrow") {
                if (current->type != TokenType::ARROW) throw runtime_error("Expected =>");
                current = current->next;
                stack.push("expect_lbrace", node);
            } else if (s == "expect_lbrace") {
                if (current->type != TokenType::LBRACE) throw runtime_error("Expected {");
                current = current->next;
                stack.push("expect_body", node);
            } else if (s == "expect_body") {
                func = dynamic_cast<FuncDef*>(node);
                func->body = parseStmtList();
                stack.push("expect_rbrace", node);
            } else if (s == "expect_rbrace") {
                if (current->type != TokenType::RBRACE) throw runtime_error("Expected }");
                current = current->next;
                return func;
            }
        }
        return func;
    }

    FuncCallExpr* parseFuncCallStmt() {
        stack.push("expect_identifier", nullptr);
        FuncCallExpr* call = nullptr;
        while (!stack.empty()) {
            ParseState* state = stack.pop();
            string s = state->state;
            ASTNode* node = state->node;
            delete state;
            if (s == "expect_identifier") {
                if (current->type != TokenType::IDENTIFIER) throw runtime_error("Expected identifier");
                call = new FuncCallExpr();
                call->name = current->value;
                current = current->next;
                stack.push("expect_lparen", call);
            } else if (s == "expect_lparen") {
                if (current->type != TokenType::LPAREN) throw runtime_error("Expected (");
                current = current->next;
                stack.push("expect_expr_list", node);
            } else if (s == "expect_expr_list") {
                call = dynamic_cast<FuncCallExpr*>(node);
                call->arguments = parseExprList();
                stack.push("expect_rparen", node);
            } else if (s == "expect_rparen") {
                if (current->type != TokenType::RPAREN) throw runtime_error("Expected )");
                current = current->next;
                stack.push("expect_semicolon", node);
            } else if (s == "expect_semicolon") {
                if (current->type != TokenType::SEMICOLON) throw runtime_error("Expected ;");
                current = current->next;
                return call;
            }
        }
        return call;
    }

    ParamList* parseParamList() {
        if (!current || current->type == TokenType::RPAREN) return nullptr;
        ParamList* head = nullptr;
        ParamList** tail = &head;
        do {
            if (current->type != TokenType::INT && current->type != TokenType::FLOAT && current->type != TokenType::STRING) {
                throw runtime_error("Expected type in parameter");
            }
            string type = current->value;
            current = current->next;
            if (current->type != TokenType::IDENTIFIER) throw runtime_error("Expected parameter name");
            string name = current->value;
            symbols.addVariable(name);
            *tail = new ParamList(new Param(type, name));
            tail = &(*tail)->next;
            current = current->next;
            if (current && current->type == TokenType::COMMA) current = current->next;
        } while (current && current->type != TokenType::RPAREN);
        return head;
    }

    ExprList* parseExprList() {
        if (!current || current->type == TokenType::RPAREN) return nullptr;
        ExprList* head = nullptr;
        ExprList** tail = &head;
        do {
            *tail = new ExprList(parseExpression());
            tail = &(*tail)->next;
            if (current && current->type == TokenType::COMMA) current = current->next;
        } while (current && current->type != TokenType::RPAREN);
        return head;
    }


    Expr* parseExpression() {
        struct ExprStackNode {
            Expr* expr;
            ExprStackNode* next;
            ExprStackNode(Expr* e) : expr(e), next(nullptr) {}
        };
        struct OpStackNode {
            TokenType op;
            OpStackNode* next;
            OpStackNode(TokenType o) : op(o), next(nullptr) {}
        };
        ExprStackNode* exprStack = nullptr;
        OpStackNode* opStack = nullptr;
        auto pushExpr = [&](Expr* e) {
            ExprStackNode* node = new ExprStackNode(e);
            node->next = exprStack;
            exprStack = node;
        };
        auto popExpr = [&]() -> Expr* {
            if (!exprStack) throw runtime_error("Expression stack underflow");
            ExprStackNode* temp = exprStack;
            exprStack = exprStack->next;
            Expr* e = temp->expr;
            delete temp;
            return e;
        };
        auto pushOp = [&](TokenType o) {
            OpStackNode* node = new OpStackNode(o);
            node->next = opStack;
            opStack = node;
        };
        auto popOp = [&]() -> TokenType {
            if (!opStack) throw runtime_error("Operator stack underflow");
            OpStackNode* temp = opStack;
            opStack = opStack->next;
            TokenType o = temp->op;
            delete temp;
            return o;
        };
        auto clearStacks = [&]() {
            while (exprStack) {
                ExprStackNode* temp = exprStack;
                exprStack = exprStack->next;
                delete temp->expr;
                delete temp;
            }
            while (opStack) {
                OpStackNode* temp = opStack;
                opStack = opStack->next;
                delete temp;
            }
        };

        //stack.push("parse_expr", nullptr);
        //while (!stack.empty()) {
        //    ParseState* state = stack.pop();
        //    string s = state->state;
        //    delete state;
        //    if (s == "parse_expr") {
                
        auto precedence = [](TokenType op) {
            switch (op) {
                case TokenType::MULT:
                case TokenType::DIV:
                    return 2;
                case TokenType::PLUS:
                case TokenType::MINUS:
                    return 1;
                case TokenType::EQ:
                case TokenType::LT:
                case TokenType::GT:
                case TokenType::LE:
                case TokenType::GE:
                    return 0;
                default:
                    return -1; // Not an operator
            }
        };

        auto applyOp = [&]() {
            if (!opStack) return;
            TokenType op = popOp();
            Expr* right = popExpr();
            Expr* left = popExpr();
            BinaryExpr* bin = new BinaryExpr();
            bin->op = op;
            bin->left = left;
            bin->right = right;
            pushExpr(bin);
        };

        Token* token = current;
        while (token && token->type != TokenType::SEMICOLON && token->type != TokenType::COMMA && token->type != TokenType::RPAREN) {
            if (token->type == TokenType::NUMBER) {
                pushExpr(new NumberExpr(token->value));
                token = token->next;
            } else if (token->type == TokenType::STRING_LITERAL) {
                pushExpr(new StringExpr(token->value));
                token = token->next;
            } else if (token->type == TokenType::IDENTIFIER) {
                if (!symbols.isDeclared(token->value)) throw runtime_error("Undefined variable: " + token->value);
                pushExpr(new VarExpr(token->value));
                token = token->next;
            } else if (token->type == TokenType::LPAREN) {
                pushOp(TokenType::LPAREN);
                token = token->next;
            } else if (token->type == TokenType::RPAREN) {
                while (opStack && opStack->op != TokenType::LPAREN) {
                    applyOp();
                }
                if (opStack) popOp(); // Remove LPAREN
                else throw runtime_error("Mismatched parentheses");
                token = token->next;
            } else if (token->type == TokenType::PLUS || token->type == TokenType::MINUS ||
                       token->type == TokenType::MULT || token->type == TokenType::DIV ||
                       token->type == TokenType::EQ || token->type == TokenType::LT ||
                       token->type == TokenType::GT || token->type == TokenType::LE ||
                       token->type == TokenType::GE || token->type == TokenType::AND ||
                       token->type == TokenType::OR) {
                while (opStack && opStack->op != TokenType::LPAREN && precedence(token->type) <= precedence(opStack->op)) {
                    applyOp();
                }
                pushOp(token->type);
                token = token->next;
            } else {
                throw runtime_error("Invalid token in expression: " + token->value);
            }
        }
        current = token;

        while (opStack) {
            applyOp();
        }

        if (!exprStack) throw runtime_error("Invalid expression");
        Expr* result = popExpr();
        if (exprStack) throw runtime_error("Invalid expression");

        clearStacks();
        return result;
    }


    StmtList* parseStmtList() {
        StmtList* head = nullptr;
        StmtList** tail = &head;
        while (current && current->type != TokenType::RBRACE && current->type != TokenType::END) {
            ASTNode* stmt = parseStatement();
            if (stmt) {
                *tail = new StmtList(stmt);
                tail = &(*tail)->next;
            }
            // Don't advance the token here as parseStatement already does that
        }
        return head;
    }
};

// Code generator
class CodeGenerator {
    Program* program;

public:
    CodeGenerator(Program* p) : program(p) {}
    ~CodeGenerator() { delete program; }

    string generate() {
        string code = "#include <iostream>\n#include <string>\nusing namespace std;\n\n";
        // Forward declare functions
        StmtList* stmt = program->statements;
        while (stmt) {
            if (FuncDef* func = dynamic_cast<FuncDef*>(stmt->stmt)) {
                code += (func->params ? func->params->param->type : "void") + " " + func->name + "(";
                ParamList* param = func->params;
                for (size_t i = 0; param; param = param->next, ++i) {
                    code += param->param->type + " " + param->param->name;
                    if (param->next) code += ", ";
                }
                code += ");\n";
            }
            stmt = stmt->next;
        }
        code += "\nint main() {\n";
        stmt = program->statements;
        while (stmt) {
            if (DeclStmt* decl = dynamic_cast<DeclStmt*>(stmt->stmt)) {
                code += "    " + decl->type + " " + decl->varName + " = " + generateExpr(decl->value) + ";\n";
            } else if (PrintStmt* print = dynamic_cast<PrintStmt*>(stmt->stmt)) {
                code += "    cout << ";
                ExprList* expr = print->expressions;
                for (size_t i = 0; expr; expr = expr->next, ++i) {
                    code += generateExpr(expr->expr);
                    if (expr->next) code += " << \" \" << ";
                }
                code += " << endl;\n";
            } else if (FuncCallExpr* call = dynamic_cast<FuncCallExpr*>(stmt->stmt)) {
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
        // Function definitions
        stmt = program->statements;
        while (stmt) {
            if (FuncDef* func = dynamic_cast<FuncDef*>(stmt->stmt)) {
                code += (func->params ? func->params->param->type : "void") + " " + func->name + "(";
                ParamList* param = func->params;
                for (size_t i = 0; param; param = param->next, ++i) {
                    code += param->param->type + " " + param->param->name;
                    if (param->next) code += ", ";
                }
                code += ") {\n";
                StmtList* body = func->body;
                while (body) {
                    if (DeclStmt* decl = dynamic_cast<DeclStmt*>(body->stmt)) {
                        code += "    " + decl->type + " " + decl->varName + " = " + generateExpr(decl->value) + ";\n";
                    } else if (PrintStmt* print = dynamic_cast<PrintStmt*>(body->stmt)) {
                        code += "    cout << ";
                        ExprList* expr = print->expressions;
                        for (size_t i = 0; expr; expr = expr->next, ++i) {
                            code += generateExpr(expr->expr);
                            if (expr->next) code += " << \" \" << ";
                        }
                        code += " << endl;\n";
                    } else if (FuncCallExpr* call = dynamic_cast<FuncCallExpr*>(body->stmt)) {
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
    string generateExpr(Expr* expr) {
        if (NumberExpr* num = dynamic_cast<NumberExpr*>(expr)) {
            return num->value;
        } else if (StringExpr* str = dynamic_cast<StringExpr*>(expr)) {
            return str->value;
        } else if (VarExpr* var = dynamic_cast<VarExpr*>(expr)) {
            return var->name;
        } else if (BinaryExpr* bin = dynamic_cast<BinaryExpr*>(expr)) {
            string op;
            switch (bin->op) {
                case TokenType::PLUS: op = "+"; break;
                case TokenType::MINUS: op = "-"; break;
                case TokenType::MULT: op = "*"; break;
                case TokenType::DIV: op = "/"; break;
                case TokenType::EQ: op = "=="; break;
                case TokenType::LT: op = "<"; break;
                case TokenType::GT: op = ">"; break;
                case TokenType::LE: op = "<="; break;
                case TokenType::GE: op = ">="; break;
                case TokenType::AND: op = "&&"; break;
                case TokenType::OR: op = "||"; break;
                default: op = "="; break;
            }
            return "(" + generateExpr(bin->left) + " " + op + " " + generateExpr(bin->right) + ")";
        } else if (FuncCallExpr* call = dynamic_cast<FuncCallExpr*>(expr)) {
            string code = call->name + "(";
            ExprList* arg = call->arguments;
            for (size_t i = 0; arg; arg = arg->next, ++i) {
                code += generateExpr(arg->expr);
                if (arg->next) code += ", ";
            }
            code += ")";
            return code;
        }
        return "";
    }
};

// ANSI color codes
const string RESET = "\033[0m";
const string RED = "\033[31m";
const string GREEN = "\033[32m";
const string YELLOW = "\033[33m";
const string BLUE = "\033[34m";
const string MAGENTA = "\033[35m";
const string CYAN = "\033[36m";
const string BOLD = "\033[1m";

// Save to file with overwrite handling
void saveToFile(const string& code, const string& filename) {
    // Create saves directory if it doesn't exist
    string outputDir = "saves";
    if (system(("mkdir -p " + outputDir).c_str()) != 0) {
        cerr << RED << "Warning: Could not create saves directory\n" << RESET;
    }
    
    string base = filename.empty() ? "program.cpp" : (filename.find(".cpp") == string::npos ? filename + ".cpp" : filename);
    int counter = 0;
    string finalName = outputDir + "/" + base;
    
    while (ifstream(finalName)) {
        cout << YELLOW << "File '" << finalName << "' exists. Overwrite? (y/n): " << RESET;
        string response;
        getline(cin, response);
        if (response == "y" || response == "Y") break;
        counter++;
        size_t pos = base.find(".cpp");
        string baseName = base.substr(0, pos);
        finalName = outputDir + "/" + baseName + "(" + to_string(counter) + ").cpp";
    }
    
    ofstream out(finalName);
    if (!out) throw runtime_error("Failed to write to " + finalName);
    out << code;
    out.close();
    cout << GREEN << "Saved to " << finalName << RESET << endl;
}

// Main compiler function
string compile(const string& source, SymbolTable& symbols) {
    Lexer lexer(source);
    TokenList* tokens = lexer.tokenize();
    Parser parser(tokens, symbols);
    Program* program = parser.parse();
    CodeGenerator generator(program);
    return generator.generate();
}

void printResLogo() {
    cout << CYAN << BOLD;
    cout << R"(
  _____                _____                         _ _           
 |  __ \              / ____|                      (_) |          
 | |__) |___  ___    | |     ___  _ __ ___  _ __ _  _| | ___ _ __ 
 |  _  // _ \/ __|   | |    / _ \| '_ ` _ \| '__  || | |/ _ \ '__|
 | | \ \  __/\__ \   | |___| (_) | | | | | | |_ | || | |\ __/ |   
 |_|  \_\___||___/    \_____\___/|_| |_| |_|_ _ __||_|_| \__| |   
                                           | |  
                                           |_|
                                           

)" << RESET << endl;
    
    cout << YELLOW << "╔══════════════════════════════════════════════════════════════╗" << RESET << endl;
    cout << YELLOW << "║ " << GREEN << "Welcome to the Res Programming Language! " << YELLOW << "                ║" << RESET << endl;
    cout << YELLOW << "║ " << BLUE << "Type " << BOLD << "\\g" << RESET << BLUE << " to compile, " << BOLD << "Ctrl+C" << RESET << BLUE << " to exit" << YELLOW << "                       ║" << RESET << endl;
    cout << YELLOW << "║ " << CYAN << "All files will be saved to the 'saves' directory" << YELLOW << "           ║" << RESET << endl;
    cout << YELLOW << "╚══════════════════════════════════════════════════════════════╝" << RESET << endl;
}
            
int main() {
    printResLogo();
    
    string input;
    string line;
    SymbolTable symbols;
    int lineCount = 0;
    
    cout << CYAN << "\n> " << RESET;
    while (true) {
        getline(cin, line);
        if (line == "\\g") {
            if (!input.empty()) {
                try {
                    // Count non-empty lines
                    int nonEmptyLines = 0;
                    istringstream iss(input);
                    string tempLine;
                    while (getline(iss, tempLine)) {
                        if (!tempLine.empty() && tempLine.find_first_not_of(" \t") != string::npos) {
                            nonEmptyLines++;
                        }
                    }
                    
                    cout << YELLOW << "\n[" << BOLD << "Compiling " << nonEmptyLines << " lines of code" << RESET << YELLOW << "]" << RESET << endl;
                    string output = compile(input, symbols);
                    cout << GREEN << "\n✓ " << BOLD << "Compilation successful!" << RESET << GREEN << " (0 errors, 0 warnings)" << RESET << endl;
                    cout << MAGENTA << "┌────────────────────────────────────────────────────────┐" << RESET << endl;
                    cout << MAGENTA << "│ " << BOLD << "Generated C++ Code:" << RESET << MAGENTA << "                                    │" << RESET << endl;
                    cout << MAGENTA << "└────────────────────────────────────────────────────────┘" << RESET << endl;
                    cout << output << endl;
                    cout << MAGENTA << "───────────────────────────────────────────────────────────" << RESET << endl;
                    cout << BLUE << "Enter output filename (default: program.cpp): " << RESET;
                    string filename;
                    getline(cin, filename);
                    saveToFile(output, filename);
                } catch (const exception& e) {
                    cout << RED << "⚠ " << BOLD << "Error:" << RESET << RED << " " << e.what() << RESET << endl;
                    cout << RED << "──────────────────────────────────────────────────────────" << RESET << endl;
                    cout << RED << "Compilation failed. Please fix the error and try again." << RESET << endl;
                }
                input.clear();
                lineCount = 0;
                cout << YELLOW << "\n[" << BOLD << "Ready for new code" << RESET << YELLOW << "]" << RESET << endl;
                cout << CYAN << "> " << RESET;
            } else {
                cout << YELLOW << "No code to compile! Please enter some code first." << RESET << endl;
                cout << CYAN << "> " << RESET;
            }
        } else {
            input += line + "\n";
            lineCount++;
            cout << CYAN << "> " << RESET;
        }
    }
    return 0;
}
