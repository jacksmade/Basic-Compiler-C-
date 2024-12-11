#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <cctype>

using namespace std;

enum TokenType {
    T_INT, T_FLOAT, T_DOUBLE, T_EQ, T_BOOL, T_CHAR,T_CHAR_LITERAL, T_ID, T_NUM, T_AGR, T_WERNA, T_WHILE, T_FOR, T_RETURN,
    T_ASSIGN, T_PLUS, T_MINUS, T_MUL, T_DIV,
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE,
    T_SEMICOLON, T_GT,T_LT, T_EOF,
};

struct Token {
    TokenType type;
    string value;
    int linenumber;
};

struct Symbol {
    string type;
    string name;
    int scopeLevel;
    Symbol() : type(""), name(""), scopeLevel(0) {}
    Symbol(string t, string n, int s) : type(t), name(n), scopeLevel(s) {}
};

struct TACInstruction {
    string op;
    string arg1;
    string arg2;
    string result;
};

class SymbolTable {
private:
    unordered_map<string, Symbol> table;

public:
    void addSymbol(const string& name, const string& type, int scopeLevel) {
        if (table.find(name) != table.end()) {
            cerr << "Error: Variable '" << name << "' is already defined." << endl;
            exit(1);
        }
        table[name] = Symbol(type, name, scopeLevel);
    }

    Symbol* getSymbol(const string& name) {
        if (table.find(name) != table.end()) {
            return &table[name];
        }
        return nullptr;
    }

    bool symbolExists(const string& name) {
        return table.find(name) != table.end();
    }
    const unordered_map<string, Symbol>& getTable() const {
        return table;
    }
};

class Lexer {
private:
    string src;
    size_t pos;
    int linenumber;

public:
    Lexer(const string& src) : src(src), pos(0), linenumber(1) {}

    vector<Token> tokenize() {
        vector<Token> tokens;
        while (pos < src.size()) {
            char current = src[pos];

            if (current == '\n') {
                linenumber++;
                pos++;
                continue;
            }
            
            if (current == '/' && src[pos + 1] == '/') {
                skipSingleLineComment();
                continue;
            }
            if (current == '=') {
                if (pos + 1 < src.size() && src[pos + 1] == '=') { // Check for ==
                    tokens.push_back(Token{T_EQ, "==", linenumber});
                    pos += 2;
                } else { // Single =
                    tokens.push_back(Token{T_ASSIGN, "=", linenumber});
                    pos++;
                }
                continue;
            }
            if (isspace(current)) {
                pos++;
                continue;
            }
            if (isdigit(current)) {
                tokens.push_back(Token{T_NUM, consumeNumber(), linenumber});
                continue;
            }
            if (current == '\'') {
            tokens.push_back(Token{T_CHAR_LITERAL, consumeCharLiteral(), linenumber});
            continue; 
            }
            if (isalpha(current)) {
                string word = consumeWord();
                if (word == "int") tokens.push_back(Token{T_INT, word, linenumber});
                else if (word == "float") tokens.push_back(Token{T_FLOAT, word, linenumber});
                else if (word == "agr") tokens.push_back(Token{T_AGR, word, linenumber});
                else if (word == "werna") tokens.push_back(Token{T_WERNA, word, linenumber});
                else if (word == "return") tokens.push_back(Token{T_RETURN, word, linenumber});
                else if (word == "double") tokens.push_back(Token{T_DOUBLE, word, linenumber});
                else if (word == "while") tokens.push_back(Token{T_WHILE, word, linenumber});
                else if (word == "for") tokens.push_back(Token{T_FOR, word, linenumber});
                else if (word == "bool") tokens.push_back(Token{T_BOOL, word, linenumber});
                else if (word == "char") tokens.push_back(Token{T_CHAR, word, linenumber});
                else tokens.push_back(Token{T_ID, word, linenumber});
                continue;
            }

            switch (current) {
                case '+': tokens.push_back(Token{T_PLUS, "+", linenumber}); break;
                case '-': tokens.push_back(Token{T_MINUS, "-", linenumber}); break;
                case '*': tokens.push_back(Token{T_MUL, "*", linenumber}); break;
                case '/': tokens.push_back(Token{T_DIV, "/", linenumber}); break;
                case '(': tokens.push_back(Token{T_LPAREN, "(", linenumber}); break;
                case ')': tokens.push_back(Token{T_RPAREN, ")", linenumber}); break;
                case '{': tokens.push_back(Token{T_LBRACE, "{", linenumber}); break;
                case '}': tokens.push_back(Token{T_RBRACE, "}", linenumber}); break;
                case ';': tokens.push_back(Token{T_SEMICOLON, ";", linenumber}); break;
                case '>': tokens.push_back(Token{T_GT, ">", linenumber}); break;
                case '<': tokens.push_back(Token{T_LT, "<", linenumber}); break;
                default: cout << "Unexpected character: " << current << endl; exit(1);
            }
            pos++;
        }
        tokens.push_back(Token{T_EOF, "", linenumber});
        return tokens;
    }

private:
    void skipSingleLineComment() {
        while (pos < src.size() && src[pos] != '\n') pos++;
        linenumber++;
    }

    string consumeNumber() {
        size_t start = pos;
        while (pos < src.size() && isdigit(src[pos])) pos++;
        return src.substr(start, pos - start);
    }
    string consumeCharLiteral() {
    pos++; // Skip the opening single quote

    if (pos >= src.size() || src[pos] == '\n') {
        cout << "Error: Unterminated character literal on line " << linenumber << endl;
        exit(1);
    }

    char character = src[pos]; // Extract the character
    string value;

    if (character == '\\') { // Handle escape sequences
        pos++; // Skip the backslash
        if (pos >= src.size()) {
            cout << "Error: Invalid escape sequence on line " << linenumber << endl;
            exit(1);
        }

        switch (src[pos]) {
            case 'n': value = "\n"; break;
            case 't': value = "\t"; break;
            case '\'': value = "'"; break;
            case '\\': value = "\\"; break;
            default:
                cout << "Error: Unknown escape sequence on line " << linenumber << endl;
                exit(1);
        }
    } else {
        value = string(1, character); // Single valid character, including digits
    }

    pos++; // Move past the character

    if (pos >= src.size() || src[pos] != '\'') {
        cout << "Error: Unterminated character literal on line " << linenumber << endl;
        exit(1);
    }

    pos++; // Skip the closing single quote
    return value;
}

    string consumeWord() {
        size_t start = pos;
        while (pos < src.size() && isalnum(src[pos])) pos++;
        return src.substr(start, pos - start);
    }
};

class Parser {
private:
    vector<Token> tokens;
    size_t pos;
    SymbolTable& symbolTable;
    vector<TACInstruction> tacInstructions;
    int tempCount = 0;

    string newTemp() { 
        return "t" + to_string(tempCount++); 
    }

    int labelCounter = 0;

    string newLabel() {
        return "L" + to_string(labelCounter++);
    }

    void emit(const string& op, const string& arg1 = "", const string& arg2 = "", const string& result = "") {
        tacInstructions.push_back({op, arg1, arg2, result});
    }

    void emitLabel(const string& label) {
        emit("", "", "", label + ":");
    }

public:
    Parser(const vector<Token>& tokens, SymbolTable& symbolTable) : tokens(tokens), symbolTable(symbolTable), pos(0) {}

    void parseProgram() {
        while (tokens[pos].type != T_EOF) {
            parseStatement();
        }
        cout << "Parsing completed successfully! No Syntax Error\n";
    }

    void printTAC() {
        cout << "\nGenerated Three-Address Code:\n";
        for (const auto& instr : tacInstructions) {
        if (instr.op == "=") { 
            // Simple assignment (e.g., a = b)
            cout << instr.result << " = " << instr.arg1 << endl;
        } else if (!instr.op.empty()) { 
            // Binary operation (e.g., a = b + c)
            cout << instr.result << " = " << instr.arg1 << " " << instr.op << " " << instr.arg2 << endl;
        } else if (!instr.arg1.empty()) {
            // Single-operand assignment (e.g., a = 5)
            cout << instr.result << " = " << instr.arg1 << endl;
        } else if (!instr.result.empty()) { 
            // Label or standalone statement (e.g., LABEL1 or GOTO LABEL2)
            cout << instr.result << endl;
        }
    }
    }
    void generateAssembly() {
        cout << "\nGenerated Assembly Code:\n";

        // Declare memory for all variables in the symbol table
        for (const auto& entry : symbolTable.getTable()) {
            cout << entry.second.name << ":\tRESW 1\n";
        }

        for (const auto& instr : tacInstructions) {
            if (instr.op == "=") {
                // Simple assignment: result = arg1
                cout << "MOV " << instr.result << ", " << instr.arg1 << endl;
            } else if (instr.op == "+" || instr.op == "-" || instr.op == "*" || instr.op == "/") {
                // Binary operations: result = arg1 op arg2
                cout << "MOV R1, " << instr.arg1 << endl;
                cout << ((instr.op == "+") ? "ADD " :
                         (instr.op == "-") ? "SUB " :
                         (instr.op == "*") ? "MUL " : "DIV ")
                     << "R1, " << instr.arg2 << endl;
                cout << "MOV " << instr.result << ", R1" << endl;
            } else if (instr.op == "IF") {
                // Conditional jumps: IF arg1 op arg2 GOTO result
                cout << "CMP " << instr.arg1 << ", " << instr.arg2 << endl;
                cout << "JE " << instr.result << endl;
            } else if (instr.op == "GOTO") {
                // Unconditional jump: GOTO result
                cout << "JMP " << instr.result << endl;
            } else if (!instr.result.empty() && instr.result.back() == ':') {
                // Labels
                cout << instr.result << endl;
            }
        }
    }

private:
    
    void parseStatement() {
        if (tokens[pos].type == T_INT ){
            parseIntDeclaration();
        }
        else if (tokens[pos].type == T_FLOAT ){
            parseFloatDeclaration();
        }
        else if (tokens[pos].type == T_DOUBLE ){
            parseDoubleDeclaration();
        }
        else if (tokens[pos].type == T_CHAR ){
            parseCharDeclaration();
        }
        else if (tokens[pos].type == T_ID) {
        if (tokens[pos + 1].type == T_ASSIGN && tokens[pos + 2].type == T_CHAR_LITERAL) {
            parseCharAssignment(); // Handle character assignments like b = '2';
        } else {
            parseAssignment();
        }
        }else if (tokens[pos].type == T_RETURN) {
            parseReturnStatement();
        } else if (tokens[pos].type == T_LBRACE) {  
            parseBlock();
        } 
         else if (tokens[pos].type == T_WHILE) {  // Added while loop parsing
        parseWhileStatement();
        } 
         else if (tokens[pos].type == T_FOR) {  // Added while loop parsing
        parseForStatement();
        } 
        else if (tokens[pos].type == T_EOF) {
        // Proper EOF handling
        return;}
        else {
            cout << "Syntax error: unexpected token " << tokens[pos].value<<"on line number : "<< tokens[pos].linenumber << endl;
            exit(1);
        }
        }

    void parseIntDeclaration() {
        expect(T_INT);
        string varName = tokens[pos].value;
        expect(T_ID);
        symbolTable.addSymbol(varName, "int", 0);
        expect(T_SEMICOLON);
    }
    void parseFloatDeclaration() {
        expect(T_FLOAT);
        string varName = tokens[pos].value;
        expect(T_ID);
        symbolTable.addSymbol(varName, "float", 0);
        expect(T_SEMICOLON);
    }
    void parseDoubleDeclaration() {
        expect(T_DOUBLE);
        string varName = tokens[pos].value;
        expect(T_ID);
        symbolTable.addSymbol(varName, "double", 0);
        expect(T_SEMICOLON);
    }
    void parseIFStatement() {
    expect(T_AGR); // 'agr' is your custom keyword for 'if'
    expect(T_LPAREN);
    
    // Parse the condition
    string conditionResult = parseExpression();
    expect(T_RPAREN);

    string trueLabel = newLabel();
    string falseLabel = newLabel();
    string endLabel = newLabel();

    // Emit a conditional jump
    emit("IF", conditionResult, "0", falseLabel);
    emitLabel(trueLabel);

    // Parse the 'if' block
    parseStatement();

    emit("GOTO", "", "", endLabel);
    emitLabel(falseLabel);

    // Check for 'else' (optional)
    if (tokens[pos].type == T_WERNA) { // 'werna' is your custom keyword for 'else'
        advance();
        parseStatement();
    }

    emitLabel(endLabel);
    }
    void parseCharDeclaration() {
         expect(T_CHAR);
        string varName = tokens[pos].value;
        expect(T_ID);
        symbolTable.addSymbol(varName, "char", 0);
        expect(T_SEMICOLON);
    }
    void parseAssignment() {
        string varName = tokens[pos].value;
        expect(T_ID);
        expect(T_ASSIGN);
        string exprResult = parseExpression();
        emit("=", exprResult, "", varName);
        expect(T_SEMICOLON);
    }
    void parseCharAssignment() {
    string varName = tokens[pos].value;
    expect(T_ID); // Expect the variable name
    expect(T_ASSIGN); // Expect the '=' operator
    string charValue = tokens[pos].value;
    expect(T_CHAR_LITERAL); // Expect a character literal

    // Emit TAC for character assignment
    emit("=", charValue, "", varName);

    expect(T_SEMICOLON); // Expect the semicolon at the end
}

    void parseWhileStatement() {
        expect(T_WHILE);
        expect(T_LPAREN);

        string conditionLabel = newLabel();
        emitLabel(conditionLabel);

        string conditionResult = parseExpression();
        expect(T_RPAREN);
        parseStatement();
        string endLabel = newLabel();
        emit("IF", conditionResult, "0", endLabel);

        parseStatement(); // Parse loop body
        emit("GOTO", "", "", conditionLabel);
        emitLabel(endLabel);
    }
    void parseReturnStatement() {
        expect(T_RETURN);
        parseExpression();
        expect(T_SEMICOLON);
    }
    void parseBlock() {
        expect(T_LBRACE);  
        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF) {
            parseStatement();
        }
        expect(T_RBRACE);  
    }
    void parseForStatement() {
        expect(T_FOR);
        expect(T_LPAREN);
 
        // Initializer
        parseAssignment();

        // Condition
        string conditionLabel = newLabel();
        emitLabel(conditionLabel);

        string conditionResult = parseExpression();
        expect(T_SEMICOLON);

        string endLabel = newLabel();
        emit("IF", conditionResult, "0", endLabel);

        // Increment
        string incrementLabel = newLabel();
        emitLabel(incrementLabel);
        parseAssignment();
        emit("GOTO", "", "", conditionLabel);

        expect(T_RPAREN);

        parseStatement(); // Parse loop body
        emit("GOTO", "", "", incrementLabel);
        emitLabel(endLabel);
    }

    string parseExpression() {
    string left = parseTerm(); // Parse the left-hand side term
    while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS || 
           tokens[pos].type == T_LT || tokens[pos].type == T_GT ||  
           tokens[pos].type == T_EQ ) {
        TokenType op = tokens[pos].type;
        advance();
        string right = parseTerm(); // Parse the right-hand side term
        string result = newTemp();
        
        // Map operator to TAC
        string tacOp;
        switch (op) {
            case T_LT: tacOp = "<"; break;
            case T_GT: tacOp = ">"; break;
            case T_EQ: tacOp = "=="; break;
            default: tacOp = (op == T_PLUS) ? "+" : "-";
        }
        
        emit(tacOp, left, right, result);
        left = result;
    }
    return left;
}

    string parseTerm() {
        string factor1 = parseFactor();
        while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV) {
            TokenType op = tokens[pos].type;
            advance();
            string factor2 = parseFactor();
            string result = newTemp();
            emit(op == T_MUL ? "*" : "/", factor1, factor2, result);
            factor1 = result;
        }
        return factor1;
    }

    string parseFactor() {
        if (tokens[pos].type == T_NUM) {
            string value = tokens[pos].value;
            advance();
            return value;
        } else if (tokens[pos].type == T_ID) {
            string varName = tokens[pos].value;
            advance();
            if (!symbolTable.symbolExists(varName)) {
                cout << "Error: Variable '" << varName << "' is not declared!" << endl;
                exit(1);
            }
            return varName;
        } else if (tokens[pos].type == T_LPAREN) {
            advance();
            string exprResult = parseExpression();
            expect(T_RPAREN);
            return exprResult;
        } else {
            cout << "Unexpected token '" << tokens[pos].value << "' on line " << tokens[pos].linenumber << endl;
            exit(1);
        }
    }

    void advance() {
        if (pos < tokens.size() - 1) pos++;
    }

    void expect(TokenType expectedType) {
        if (tokens[pos].type != expectedType) {
            cout << "Expected token '" <<  tokenTypeToString(expectedType) << "' but found '" << tokens[pos].value << "' on line " << tokens[pos].linenumber << endl;
            exit(1);
        }
        advance();
    }
    string tokenTypeToString(TokenType type) {
        switch (type) {
            case T_INT: return "int";
            case T_FLOAT: return "float";
            case T_ID: return "identifier";
            case T_NUM: return "number";
            case T_AGR: return "if";
            case T_WERNA: return "else";
            case T_RETURN: return "return";
            case T_ASSIGN: return "=";
            case T_PLUS: return "+";
            case T_MINUS: return "-";
            case T_MUL: return "*";
            case T_DIV: return "/";
            case T_LPAREN: return "(";
            case T_RPAREN: return ")";
            case T_LBRACE: return "{";
            case T_RBRACE: return "}";
            case T_SEMICOLON: return ";";
            case T_GT: return ">";
            case T_LT: return ">";
            case T_WHILE: return "while";  // Added while keyword
            case T_FOR: return "for";  
            case T_EOF: return "EOF";
            default: return "unknown";
        }
    }
};

int main() {
    string code;
    ifstream inputFile("myfile.txt");
    if (inputFile) {
        stringstream buffer;
        buffer << inputFile.rdbuf();
        code = buffer.str();
    } else {
        cout << "Error reading file!" << endl;
        return 1;
    }

    SymbolTable symbolTable;
    Lexer lexer(code);
    vector<Token> tokens = lexer.tokenize();
    Parser parser(tokens, symbolTable);
    parser.parseProgram();
    parser.printTAC();
    parser.generateAssembly();
    return 0;
}
