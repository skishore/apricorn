#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

struct Diagnostic {
  size_t pos;
  std::string error;
};

// Quick-and-dirty arena-allocated trie.

namespace trie {

struct Node {
  std::array<size_t, 128> children;
};

struct Trie {
  explicit Trie(const std::vector<std::string>& entries) {
    nodes.emplace_back();
    for (const auto& entry : entries) addEntry(entry);
  }

  void addEntry(const std::string& entry) {
    size_t current = 0;
    for (const char ch : entry) {
      auto& node = nodes[current];
      const uint8_t index = static_cast<uint8_t>(ch) + 1;
      assert(index < node.children.size());
      const size_t next = node.children[index];
      if (next == 0) {
        node.children[index] = current = nodes.size();
        nodes.emplace_back();
      } else {
        current = next;
      }
    }
    nodes[current].children[0] = 1;
  }

  size_t match(const std::string& input, size_t pos) const {
    size_t current = 0;
    size_t last_match = pos;
    const size_t size = input.size();
    for (size_t end = pos; end < size; end++) {
      const auto& node = nodes[current];
      const uint8_t index = static_cast<uint8_t>(input[end]) + 1;
      assert(index <= node.children.size());
      current = node.children[index];
      if (current == 0) break;
      if (nodes[current].children[0] == 1) last_match = end + 1;
    }
    return last_match - pos;
  }

  std::string show() const {
    std::stringstream result;

    const std::function<void(size_t, size_t)> recurse =
        [&](auto index, auto depth) {
      const auto& node = nodes[index];
      const std::string spacer = std::string(2 * depth, ' ');
      result << spacer << "Terminal: " << node.children[0] << "\n";
      for (size_t i = 1; i < node.children.size(); i++) {
        const size_t child = node.children[i];
        if (child == 0) continue;
        result << spacer << "Char: " << static_cast<char>(i - 1) << "\n";
        recurse(child, depth + 1);
      }
    };
    recurse(0, 0);

    return result.str();
  }

  std::vector<Node> nodes;
};

const Trie& keywordTrie() {
  static const Trie result{{
    "break", "const", "continue", "class", "else", "export",
    "extends", "for", "if", "import", "private", "let", "new",
    "of", "return", "type", "while",
  }};
  return result;
};

const Trie& symbolTrie() {
  static const Trie result{{
    "+", "+=", "-", "-=", "*", "*=", "/", "/=", "%", "**",
    "<", "<=", ">", ">=", "==", "!=", "===", "!==", "=",
    "(", ")", "[", "]", "{", "}", "=>", "?", ":", ".", ",", ";",
    "!", "~", "|", "&", "^", "&&", "||", "??", "<<", ">>", "++", "--",
  }};
  return result;
};

} // namespace trie

// Lexer routines.

namespace lexer {

#define TOKEN_TYPE \
  X(Symbol)        \
  X(Keyword)       \
  X(Identifier)    \
  X(DblLiteral)    \
  X(IntLiteral)    \
  X(StrLiteral)    \
  X(TemplateStart) \
  X(TemplateMid)   \
  X(TemplateEnd)   \

enum class TokenType : uint8_t {
#define X(name) name,
  TOKEN_TYPE
#undef X
};

const char* desc(TokenType type) {
  switch (type) {
#define X(name) case TokenType::name: return #name;
  TOKEN_TYPE
#undef X
  }
};

#undef TOKEN_TYPE

struct Token {
  TokenType type;
  std::string_view text;
};

struct LexerResult {
  std::vector<Token> tokens;
  std::vector<Diagnostic> diagnostics;
};

std::vector<Token> lex(
    const std::string& input, std::vector<Diagnostic>* diagnostics) {
  using T = TokenType;

  size_t i = 0;
  std::vector<Token> result;
  std::vector<bool> braceStack;
  const size_t size = input.size();

  const auto consume = [&](char a) {
    const auto result = input[i] == a;
    if (result) i++;
    return result;
  };

  const auto consumeAll = [&](size_t n, const char* x) {
    for (size_t j = 0; j < n; j++) {
      if (input[i + j] != x[j]) return false;
    }
    i += n;
    return true;
  };

  const auto consumeTrie = [&](const trie::Trie& trie) {
    const auto result = trie.match(input, i);
    i += result;
    return result;
  };

  const auto digit = [](char ch) {
    return '0' <= ch && ch <= '9';
  };

  const auto binDigit = [](char ch) {
    return ch == '0' || ch == '1';
  };

  const auto hexDigit = [](char ch) {
    return ('0' <= ch && ch <= '9') ||
           ('a' <= ch && ch <= 'f') ||
           ('A' <= ch && ch <= 'F');
  };

  const auto identifierStart = [](char ch) {
    return ('a' <= ch && ch <= 'z') ||
           ('A' <= ch && ch <= 'Z') ||
           ch == '_' || ch == '$';
  };

  const auto identifier = [&](char ch) {
    return identifierStart(ch) || digit(ch);
  };

  const auto makeToken = [&](TokenType type, size_t pos, size_t end) -> Token {
    return {type, {input.data() + pos, end - pos}};
  };

  const auto parseNumber = [&]() -> Token {
    const size_t pos = i;
    if (input[i] == '0') {
      if (input[i + 1] == 'x' && hexDigit(input[i + 2])) {
        for (i += 3; hexDigit(input[i]); i++) {}
        return makeToken(T::IntLiteral, pos, i);
      } else if (input[i + 1] == 'b' && binDigit(input[i + 2])) {
        for (i += 3; binDigit(input[i]); i++) {}
        return makeToken(T::IntLiteral, pos, i);
      } else if (digit(input[i + 1])) {
        i += 1;
        return makeToken(T::IntLiteral, pos, i);
      }
    }

    while (digit(input[i])) i++;

    const char ch = input[i];
    if (ch == '.') {
      for (i++; digit(input[i]); i++) {}
      return makeToken(T::DblLiteral, pos, i);
    } else if (ch == 'e' || ch == 'E') {
      if (input[i + 1] == '0') {
        i += 2;
        return makeToken(T::DblLiteral, pos, i);
      }
      for (i++; digit(input[i]); i++) {}
      return makeToken(T::DblLiteral, pos, i);
    }
    return makeToken(T::IntLiteral, pos, i);
  };

  const auto parseHexDigits = [&](size_t offset, size_t n) -> bool {
    for (size_t i = 0; i < n; i++) {
      if (!hexDigit(input[offset + i])) return false;
    }
    i += offset + n;
    return true;
  };

  const auto parseCharacter = [&](char quote) -> bool {
    const auto ch = input[i];
    if (ch == '\\') {
      const auto next = input[i + 1];
      if (next == '\\' || next == '0' || next == '"' || next == '\'' ||
          next == 'b' || next == 'n' || next == 'r' || next == 't') {
        return i += 2;
      } else if ((next == 'x' && parseHexDigits(2, 2)) ||
                 (next == 'u' && parseHexDigits(2, 4))) {
        return true;
      }
      diagnostics->push_back({i, "Unknown string escape sequence"});
      return i += 1;
    } else if (ch != 0 && ch != '\n' && ch != '\r' && ch != quote) {
      return i += 1;
    }
    return false;
  };

  const auto parseString = [&](char quote) -> Token {
    const size_t pos = i;
    i += 1;
    while (parseCharacter(quote)) {}
    if (input[i] != quote) {
      diagnostics->push_back({i, "Unterminated string literal"});
    } else {
      i++;
    }
    return makeToken(T::StrLiteral, pos, i);
  };

  const auto parseTemplateCharacter = [&]() -> bool {
    const auto ch = input[i];
    if (ch == '\\') {
      const auto next = input[i + 1];
      if (next == '`' || next == '$') return i += 2;
      diagnostics->push_back({i, "Unknown template escape sequence"});
      return i += 1;
    } else if (ch != 0 && ch != '`' && !(ch == '$' && input[i + 1] == '{')) {
      return i += 1;
    }
    return false;
  };

  const auto parseTemplate = [&](char quote) -> Token {
    const size_t pos = i;
    i += 1;
    while (parseTemplateCharacter()) {}
    if (input[i] == '`') {
      i += 1;
      const auto type = quote == '`' ? T::StrLiteral : T::TemplateEnd;
      return makeToken(type, pos, i);
    } else if (input[i] == '$' && input[i + 1] == '{') {
      i += 2;
      const auto type = quote == '`' ? T::TemplateStart : T::TemplateMid;
      return makeToken(type, pos, i);
    }
    diagnostics->push_back({i, "Unterminated template literal"});
    const auto type = quote == '`' ? T::TemplateStart : T::TemplateMid;
    return makeToken(type, pos, i);
  };

  const auto skipWhitespace = [&]{
    while (i < size) {
      const char ch = input[i];
      if (ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t') {
        i++;
      } else if (consumeAll(2, "//")) {
        for (; i < size; i++) {
          if (consume('\n')) break;
        }
      } else if (consumeAll(2, "/*")) {
        auto ok = false;
        for (i += 2; i < size && !ok; i++) {
          ok = consumeAll(2, "*/");
        }
        if (!ok) diagnostics->push_back({i, "Unterminated /* comment"});
      } else {
        return;
      }
    }
  };

  while (i < size) {
    skipWhitespace();
    if (i == size) break;

    const size_t pos = i;
    const char ch = input[i];

    if (ch == '`') {
      result.push_back(parseTemplate(ch));
      if (result.back().type == T::TemplateStart) braceStack.push_back(true);
      continue;
    } else if (ch == '{') {
      braceStack.push_back(false);
      result.push_back(makeToken(T::Symbol, pos, ++i));
      continue;
    } else if (ch == '}' && !braceStack.empty()) {
      if (braceStack.back()) {
        result.push_back(parseTemplate(ch));
        if (result.back().type == T::TemplateEnd) braceStack.pop_back();
      } else {
        result.push_back(makeToken(T::Symbol, pos, ++i));
        braceStack.pop_back();
      }
      continue;
    }

    const size_t keywordMatched = trie::keywordTrie().match(input, i);
    if (keywordMatched && !identifier(input[i + keywordMatched])) {
      i += keywordMatched;
      result.push_back(makeToken(T::Keyword, pos, i));
    } else if (ch == '"' || ch == '\'') {
      result.push_back(parseString(ch));
    } else if (identifierStart(ch)) {
      for (i++; identifier(input[i]); i++) {}
      result.push_back(makeToken(T::Identifier, pos, i));
    } else if (digit(ch) || (ch == '.' && digit(input[i + 1]))) {
      result.push_back(parseNumber());
      if (identifier(input[i])) {
        const char* error = (
          "A numeric literal cannot be immediately followed by an "
          "identifier, keyword, or numeric literal."
        );
        diagnostics->push_back({i, error});
        for (i++; identifier(input[i]); i++) {}
      }
    } else if (const size_t matched = consumeTrie(trie::symbolTrie())) {
      result.push_back(makeToken(T::Symbol, pos, i));
    } else {
      const auto error = std::string("Unknown symbol: ") + ch;
      diagnostics->push_back({i++, error});
    }
  }
  return result;
}

} // namespace lexer

// Parser routines.

namespace ast {

template <typename T> using Ptr = std::unique_ptr<T>;

#define TYPE_TYPES  \
  X(IdentifierType) \
  X(ArrayType)      \
  X(TupleType)      \
  X(UnionType)      \
  X(ObjectType)     \
  X(GenericType)    \
  X(ClosureType)    \

enum class TypeKind : uint8_t {
#define X(name) name,
  TYPE_TYPES
#undef X
};

const char* desc(TypeKind type) {
  switch (type) {
#define X(name) case TypeKind::name: return #name;
  TYPE_TYPES
#undef X
  }
};

#undef TYPE_TYPES

#define EXPR_TYPES       \
  X(ErrorExpr)           \
  X(BinaryOpExpr)        \
  X(UnaryPrefixOpExpr)   \
  X(UnarySuffixOpExpr)   \
  X(ArrayExpr)           \
  X(ObjectExpr)          \
  X(ClosureExpr)         \
  X(TernaryExpr)         \
  X(TemplateExpr)        \
  X(AssignmentExpr)      \
  X(IdentifierExpr)      \
  X(DblLiteralExpr)      \
  X(IntLiteralExpr)      \
  X(StrLiteralExpr)      \
  X(FieldAccessExpr)     \
  X(IndexAccessExpr)     \
  X(FunctionCallExpr)    \
  X(ConstructorCallExpr) \

enum class ExprKind : uint8_t {
#define X(name) name,
  EXPR_TYPES
#undef X
};

const char* desc(ExprKind type) {
  switch (type) {
#define X(name) case ExprKind::name: return #name;
  EXPR_TYPES
#undef X
  }
};

#undef EXPR_TYPES

#define STATEMENT_TYPES        \
  X(IfStatement)               \
  X(ExprStatement)             \
  X(EmptyStatement)            \
  X(BlockStatement)            \
  X(ForEachStatement)          \
  X(ForLoopStatement)          \
  X(WhileLoopStatement)        \
  X(ReturnStatement)           \
  X(BreakStatement)            \
  X(ContinueStatement)         \
  X(DeclarationStatement)      \
  X(ClassDeclarationStatement) \
  X(TypeAliasStatement)        \

enum class StatementKind : uint8_t {
#define X(name) name,
  STATEMENT_TYPES
#undef X
};

const char* desc(StatementKind type) {
  switch (type) {
#define X(name) case StatementKind::name: return #name;
  STATEMENT_TYPES
#undef X
  }
};

#undef STATEMENT_TYPES

struct Node {
  Node() {}
  virtual ~Node() {}
  Node(const Node& o) = delete;
  Node& operator=(const Node& o) = delete;
  Node(Node&& o) = default;
  Node& operator=(Node&& o) = default;

  virtual const char* describe() const = 0;

  std::vector<Ptr<Node>> children;
  std::string_view source;
};

#define BASIC_NODE(name)                                 \
  struct name##Node : public Node {                      \
    const char* describe() const final { return #name; } \
  };

BASIC_NODE(Identifier)
BASIC_NODE(Keyword)
BASIC_NODE(Operator)
BASIC_NODE(Path)
BASIC_NODE(Template)

#undef BASIC_NODE

struct BlockStatementNode;
struct ExprNode;
struct TypeNode;

struct ArgDefinitionNode : public Node {
  const char* describe() const final { return "ArgDefinition"; }

  const IdentifierNode* name;
  const TypeNode* type = nullptr;
};

struct CallArgsNode : public Node {
  const char* describe() const final { return "CallArgs"; }

  std::vector<const ExprNode*> args;
};

struct NameTypePairNode : public Node {
  NameTypePairNode(const char* description_) : description(description_) {}

  const char* describe() const final { return description; }

  const char* description;
  const IdentifierNode* name;
  const TypeNode* type;
};

struct ObjectItemNode : public Node {
  const char* describe() const final { return "ObjectItem"; }

  const IdentifierNode* name;
  const ExprNode* expr;
};

struct ClassMemberNode : public Node {
  const char* describe() const final { return "ClassMember"; }

  const KeywordNode* access = nullptr;
  const IdentifierNode* name;
  const TypeNode* type = nullptr;
  const ExprNode* init = nullptr;
};

struct ClassMethodNode : public Node {
  const char* describe() const final { return "ClassMethod"; }

  const KeywordNode* access = nullptr;
  const IdentifierNode* name;
  std::vector<const ArgDefinitionNode*> args;
  const TypeNode* result = nullptr;
  const BlockStatementNode* blockBody = nullptr;
  const ExprNode* exprBody = nullptr;
};

struct TypeLHSNode : public Node {
  const IdentifierNode* base;
  std::vector<const IdentifierNode*> generics;
};

// Type

struct TypeNode : public Node {
  protected:
    TypeNode(TypeKind kind_) : kind(kind_) {}

  public:
    const char* describe() const final { return desc(kind); }

    TypeKind kind;
};

struct IdentifierTypeNode : public TypeNode {
  IdentifierTypeNode() : TypeNode(TypeKind::IdentifierType) {}
};

struct ArrayTypeNode : public TypeNode {
  ArrayTypeNode() : TypeNode(TypeKind::ArrayType) {}

  const TypeNode* element;
};

struct TupleTypeNode : public TypeNode {
  TupleTypeNode() : TypeNode(TypeKind::TupleType) {}

  std::vector<const TypeNode*> elements;
};

struct UnionTypeNode : public TypeNode {
  UnionTypeNode() : TypeNode(TypeKind::UnionType) {}

  std::vector<const TypeNode*> options;
};

struct ObjectTypeNode : public TypeNode {
  ObjectTypeNode() : TypeNode(TypeKind::ObjectType) {}

  std::vector<const NameTypePairNode*> items;
};

struct GenericTypeNode : public TypeNode {
  GenericTypeNode() : TypeNode(TypeKind::GenericType) {}

  const IdentifierTypeNode* base;
  std::vector<const TypeNode*> generics;
};

struct ClosureTypeNode : public TypeNode {
  ClosureTypeNode() : TypeNode(TypeKind::ClosureType) {}

  std::vector<const NameTypePairNode*> args;
  const TypeNode* result;
};

// Expr

struct ExprNode : public Node {
  protected:
    ExprNode(ExprKind kind_) : kind(kind_) {}

  public:
    const char* describe() const final { return desc(kind); }

    ExprKind kind;
};

struct ErrorExprNode : public ExprNode {
  ErrorExprNode() : ExprNode(ExprKind::ErrorExpr) {}
};

struct BinaryOpExprNode : public ExprNode {
  BinaryOpExprNode() : ExprNode(ExprKind::BinaryOpExpr) {}

  const OperatorNode* op;
  const ExprNode* lhs;
  const ExprNode* rhs;
};

struct UnaryPrefixOpExprNode : public ExprNode {
  UnaryPrefixOpExprNode() : ExprNode(ExprKind::UnaryPrefixOpExpr) {}

  const OperatorNode* op;
  const ExprNode* expr;
};

struct UnarySuffixOpExprNode : public ExprNode {
  UnarySuffixOpExprNode() : ExprNode(ExprKind::UnarySuffixOpExpr) {}

  const OperatorNode* op;
  const ExprNode* expr;
};

struct ArrayExprNode : public ExprNode {
  ArrayExprNode() : ExprNode(ExprKind::ArrayExpr) {}

  std::vector<const ExprNode*> elements;
};

struct ObjectExprNode : public ExprNode {
  ObjectExprNode() : ExprNode(ExprKind::ObjectExpr) {}

  std::vector<const ObjectItemNode*> items;
};

struct ClosureExprNode : public ExprNode {
  ClosureExprNode() : ExprNode(ExprKind::ClosureExpr) {}

  std::vector<const ArgDefinitionNode*> args;
  const TypeNode* result = nullptr;
  const BlockStatementNode* blockBody = nullptr;
  const ExprNode* exprBody = nullptr;
};

struct TernaryExprNode : public ExprNode {
  TernaryExprNode() : ExprNode(ExprKind::TernaryExpr) {}

  const ExprNode* cond;
  const ExprNode* lhs;
  const ExprNode* rhs;
};

struct TemplateExprNode : public ExprNode {
  TemplateExprNode() : ExprNode(ExprKind::TemplateExpr) {}

  struct TemplatePair {
    const ExprNode* expr;
    const TemplateNode* text;
  };
  const TemplateNode* prefix;
  std::vector<TemplatePair> suffixes;
};

struct AssignmentExprNode : public ExprNode {
  AssignmentExprNode() : ExprNode(ExprKind::AssignmentExpr) {}

  const OperatorNode* op;
  const ExprNode* lhs; // TODO: check for a valid expression
  const ExprNode* rhs;
};

struct IdentifierExprNode : public ExprNode {
  IdentifierExprNode() : ExprNode(ExprKind::IdentifierExpr) {}
};

struct DblLiteralExprNode : public ExprNode {
  DblLiteralExprNode() : ExprNode(ExprKind::DblLiteralExpr) {}
};

struct IntLiteralExprNode : public ExprNode {
  IntLiteralExprNode() : ExprNode(ExprKind::IntLiteralExpr) {}
};

struct StrLiteralExprNode : public ExprNode {
  StrLiteralExprNode() : ExprNode(ExprKind::StrLiteralExpr) {}
};

struct FieldAccessExprNode : public ExprNode {
  FieldAccessExprNode() : ExprNode(ExprKind::FieldAccessExpr) {}

  const ExprNode* base;
  const IdentifierNode* field;
};

struct IndexAccessExprNode : public ExprNode {
  IndexAccessExprNode() : ExprNode(ExprKind::IndexAccessExpr) {}

  const ExprNode* base;
  const ExprNode* index;
};

struct FunctionCallExpr : public ExprNode {
  FunctionCallExpr() : ExprNode(ExprKind::FunctionCallExpr) {}

  const ExprNode* fn;
  const CallArgsNode* args;
};

struct ConstructorCallExprNode : public ExprNode {
  ConstructorCallExprNode() : ExprNode(ExprKind::ConstructorCallExpr) {}

  const IdentifierNode* cls;
  const CallArgsNode* args;
};

// Statement

struct StatementNode : public Node {
  StatementNode() : kind(StatementKind::TypeAliasStatement) {}

  protected:
    StatementNode(StatementKind kind_) : kind(kind_) {}

  public:
    const char* describe() const final { return desc(kind); }

    StatementKind kind;
};

struct IfStatementNode : public StatementNode {
  IfStatementNode() : StatementNode(StatementKind::IfStatement) {}

  struct CondClause {
    const ExprNode* cond;
    const StatementNode* then;
  };
  std::vector<CondClause> cases;
  const StatementNode* elseCase = nullptr;
};

struct ExprStatementNode : public StatementNode {
  ExprStatementNode() : StatementNode(StatementKind::ExprStatement) {}

  const ExprNode* expr;
};

struct EmptyStatementNode : public StatementNode {
  EmptyStatementNode() : StatementNode(StatementKind::EmptyStatement) {}
};

struct BlockStatementNode : public StatementNode {
  BlockStatementNode() : StatementNode(StatementKind::BlockStatement) {}

  std::vector<const StatementNode*> statements;
};

struct DeclarationStatementNode : public StatementNode {
  DeclarationStatementNode()
      : StatementNode(StatementKind::DeclarationStatement) {}

  const KeywordNode* keyword;
  const ExprNode* root;
  const TypeNode* type = nullptr;
  const ExprNode* expr;
};

struct ClassDeclarationStatementNode : public StatementNode {
  ClassDeclarationStatementNode()
      : StatementNode(StatementKind::ClassDeclarationStatement) {}

  const IdentifierNode* name;
  const IdentifierNode* base = nullptr;
  std::vector<const ClassMemberNode*> members;
  std::vector<const ClassMethodNode*> methods;
};

struct ForEachStatementNode : public StatementNode {
  ForEachStatementNode() : StatementNode(StatementKind::ForEachStatement) {}

  struct Initializer {
    const KeywordNode* keyword;
    const ExprNode* root;
    const TypeNode* type = nullptr;
    const ExprNode* expr;
  };
  Initializer init;
  const StatementNode* body;
};

struct ForLoopStatementNode : public StatementNode {
  ForLoopStatementNode() : StatementNode(StatementKind::ForLoopStatement) {}

  const StatementNode* init;
  const ExprNode* cond;
  const StatementNode* post;
  const StatementNode* body;
};

struct WhileLoopStatementNode : public StatementNode {
  WhileLoopStatementNode() : StatementNode(StatementKind::WhileLoopStatement) {}

  const ExprNode* cond;
  const StatementNode* body;
};

struct ReturnStatementNode : public StatementNode {
  ReturnStatementNode() : StatementNode(StatementKind::ReturnStatement) {}

  const ExprNode* expr = nullptr;
};

struct BreakStatementNode : public StatementNode {
  BreakStatementNode() : StatementNode(StatementKind::BreakStatement) {}
};

struct ContinueStatementNode : public StatementNode {
  ContinueStatementNode() : StatementNode(StatementKind::ContinueStatement) {}
};

struct TypeAliasStatementNode : public StatementNode {
  TypeAliasStatementNode() : StatementNode(StatementKind::TypeAliasStatement) {}

  const IdentifierNode* lhs;
  const TypeNode* rhs;
};

struct ProgramNode : public Node {
  const char* describe() const final { return "Program"; }

  std::vector<const StatementNode*> statements;
};

} // namespace ast

namespace ops {

using Symbol = uint32_t;
template <typename T>
using SymbolMap = std::unordered_map<Symbol, T>;
using SymbolSet = std::unordered_set<Symbol>;

// Lower precedence means tighter binding.
//
// Precedence is a range: it is ambiguous for ops like bitwise ops, etc.
// We will force the user to place parentheses in an expr like a + b | c.
//
// Some ops support repetition. The ops that do are all left-associative.
struct Precedence {
  uint8_t glb;
  uint8_t lub;
  bool repeat;
};

Symbol key(const std::string_view& symbol) {
  Symbol key = 0;
  assert(symbol.size() <= sizeof(Symbol));
  memcpy(&key, symbol.data(), symbol.size());
  return key;
}

const SymbolSet& assignment() {
  static const SymbolSet result{
    key("="), key("+="), key("-="), key("*="), key("/="),
  };
  return result;
}

const SymbolSet& preops() {
  static const SymbolSet result{
    key("!"), key("~"), key("+"), key("-"), key("++"), key("--"),
  };
  return result;
}

const SymbolSet& postops() {
  static const SymbolSet result{key("++"), key("--")};
  return result;
}

const SymbolMap<Precedence>& binops() {
  static const SymbolMap<Precedence> result{
    // Basic arithmetic ops.
    {key("**"),  {1, 1, 0}},
    {key("*"),   {2, 2, 1}},
    {key("/"),   {2, 2, 1}},
    {key("+"),   {3, 3, 1}},
    {key("-"),   {3, 3, 1}},
    // Bit-ops with confusing precedence.
    {key("%"),   {0, 4, 0}},
    {key("<<"),  {0, 4, 0}},
    {key(">>"),  {0, 4, 0}},
    // Comparisons and equality ops.
    {key("<"),   {5, 5, 0}},
    {key("<="),  {5, 5, 0}},
    {key(">"),   {5, 5, 0}},
    {key(">="),  {5, 5, 0}},
    {key("=="),  {6, 6, 0}},
    {key("!="),  {6, 6, 0}},
    {key("==="), {6, 6, 0}},
    {key("!=="), {6, 6, 0}},
    {key("&&"),  {7, 8, 1}},
    {key("||"),  {7, 8, 1}},
    // Bit-ops with confusing precedence.
    {key("&"),   {0, 9, 1}},
    {key("|"),   {0, 9, 1}},
    {key("^"),   {0, 9, 1}},
    {key("??"),  {0, 9, 1}},
  };
  return result;
}

} // namespace ops

namespace parser {

using namespace ast;
using namespace lexer;
using T = TokenType;
using SK = StatementKind;
using TK = TypeKind;

struct Env {
  const std::string& input;
  const std::vector<Token>& tokens;
  std::vector<Diagnostic>* diagnostics;
  size_t i;
};

template <typename T>
const T* append(Node& node, Ptr<T> child) {
  const T* result = child.get();
  node.children.push_back(std::move(child));
  return result;
}

size_t cursor(Env* env) {
  return env->i < env->input.size()
    ? static_cast<size_t>(env->tokens[env->i].text.data() - env->input.data())
    : env->input.size();
}

std::string_view source(Env* env, size_t pos, size_t end) {
  return {env->input.data() + pos, end - pos};
}

bool ahead(Env* env, size_t i, TokenType type, const char* text = nullptr) {
  if (env->i + i == env->tokens.size()) return false;
  const auto& token = env->tokens[env->i + i];
  return token.type == type && (!text || token.text == text);
}

bool check(Env* env, TokenType type, const char* text = nullptr) {
  return ahead(env, 0, type, text);
}

bool consume(Env* env, TokenType type, const char* text = nullptr) {
  const auto result = check(env, type, text);
  if (result) env->i++;
  return result;
}

bool require(Env* env, const char* message,
             TokenType type, const char* text = nullptr) {
  if (consume(env, type, text)) return true;
  env->diagnostics->push_back({cursor(env), message});
  return false;
}

template <typename T>
Ptr<T> parseToken(Env* env, TokenType tt, const char* error) {
  auto result = std::make_unique<T>();
  if (require(env, error, tt)) {
    result->source = env->tokens[env->i - 1].text;
  }
  return result;
}

Ptr<PathNode> parsePath(Env* env) {
  return parseToken<PathNode>(env, T::StrLiteral, "Expected: path");
}

Ptr<IdentifierNode> parseIdentifier(Env* env) {
  return parseToken<IdentifierNode>(env, T::Identifier, "Expected: identifier");
}

Ptr<KeywordNode> parseKeyword(Env* env) {
  return parseToken<KeywordNode>(env, T::Keyword, "Expected: keyword");
}

Ptr<OperatorNode> parseOperator(Env* env) {
  return parseToken<OperatorNode>(env, T::Symbol, "Expected: operator");
}

Ptr<IdentifierTypeNode> parseIdentifierType(Env* env) {
  return parseToken<IdentifierTypeNode>(env, T::Identifier, "Expected: type");
}

// Type grammar.

Ptr<TypeNode> parseType(Env* env);

Ptr<NameTypePairNode> parseNameTypePair(Env* env, const char* description) {
  auto result = std::make_unique<NameTypePairNode>(description);
  result->name = append(*result, parseIdentifier(env));
  require(env, "Expected: :", T::Symbol, ":");
  result->type = append(*result, parseType(env));
  return result;
}

Ptr<ClosureTypeNode> parseClosureType(Env* env) {
  const auto parseClosureBody = [&](Ptr<ClosureTypeNode> result) {
    require(env, "Expected: =>", T::Symbol, "=>");
    result->result = append(*result, parseType(env));
    return result;
  };

  const auto checkForArrow = [&](size_t i) {
    return ahead(env, i, T::Symbol, ")") && ahead(env, i + 1, T::Symbol, "=>");
  };

  const auto checkForArgList = [&](size_t i) {
    if (checkForArrow(i)) return true;
    if (!ahead(env, i, T::Identifier)) return false;
    return ahead(env, i + 1, T::Symbol, ",") ||
           ahead(env, i + 1, T::Symbol, ":") ||
           checkForArrow(i + 1);
  };

  if (ahead(env, 0, T::Symbol, "(") && checkForArgList(1)) {
    require(env, "Expected: (", T::Symbol, "(");
    auto result = std::make_unique<ClosureTypeNode>();
    if (consume(env, T::Symbol, ")")) return parseClosureBody(std::move(result));
    do {
      result->args.push_back(append(*result, parseNameTypePair(env, "Arg")));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")"));
    require(env, "Expected: )", T::Symbol, ")");
    return parseClosureBody(std::move(result));
  }

  return nullptr;
}

Ptr<TypeNode> parseQualifiedType(Env* env, bool lhs) {
  auto result = parseIdentifierType(env);
  if (!consume(env, T::Symbol, "<")) return result;

  auto generic = std::make_unique<GenericTypeNode>();
  generic->base = append(*generic, std::move(result));
  // NOCOMMIT
  //const std::function<Ptr<Node>(Env*)> fn = lhs ? parseGeneric : parseType;
  const std::function<Ptr<TypeNode>(Env*)> fn = parseType;
  do {
    generic->generics.push_back(append(*generic, fn(env)));
  } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ">"));
  require(env, "Expected: >", T::Symbol, ">");
  return generic;
}

Ptr<TypeNode> parseRootType(Env* env) {
  if (auto closure = parseClosureType(env)) return closure;

  if (consume(env, T::Symbol, "(")) {
    auto result = parseType(env);
    require(env, "Expected: )", T::Symbol, ")");
    return result;
  }

  if (consume(env, T::Symbol, "[")) {
    auto result = std::make_unique<TupleTypeNode>();
    if (consume(env, T::Symbol, "]")) return result;
    do {
      result->elements.push_back(append(*result, parseType(env)));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "]"));
    require(env, "Expected: ]", T::Symbol, "]");
    return result;
  }

  if (consume(env, T::Symbol, "{")) {
    auto result = std::make_unique<ObjectTypeNode>();
    if (consume(env, T::Symbol, "}")) return result;
    do {
      result->items.push_back(append(*result, parseNameTypePair(env, "Item")));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "}"));
    require(env, "Expected: }", T::Symbol, "}");
    return result;
  }

  return parseQualifiedType(env, false);
}

Ptr<TypeNode> parseTermType(Env* env) {
  auto result = parseRootType(env);
  if (consume(env, T::Symbol, "[")) {
    require(env, "Expected: ]", T::Symbol, "]");
    auto list = std::make_unique<ArrayTypeNode>();
    list->element = append(*list, std::move(result));
    result = std::move(list);
  }
  return result;
}

Ptr<TypeNode> parseType(Env* env) {
  auto result = parseTermType(env);
  if (!consume(env, T::Symbol, "|")) return result;

  auto ut = std::make_unique<UnionTypeNode>();
  ut->options.push_back(append(*ut, std::move(result)));
  do {
    ut->options.push_back(append(*ut, parseTermType(env)));
  } while (consume(env, T::Symbol, "|"));
  return ut;
}

// Expression grammar.

Ptr<ExprNode> parseExpr(Env* env);
Ptr<StatementNode> parseStatement(Env* env);
Ptr<BlockStatementNode> parseBlockStatement(Env* env);

Ptr<CallArgsNode> parseCallArgs(Env* env) {
  auto result = std::make_unique<CallArgsNode>();
  if (consume(env, T::Symbol, ")")) return result;
  do {
    result->args.push_back(append(*result, parseExpr(env)));
  } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")"));
  require(env, "Expected: )", T::Symbol, ")");
  return result;
}

Ptr<ObjectItemNode> parseObjectItem(Env* env) {
  auto result = std::make_unique<ObjectItemNode>();
  result->name = append(*result, parseIdentifier(env));
  if (consume(env, T::Symbol, ":")) {
    result->expr = append(*result, parseExpr(env));
  } else {
    auto identifier = std::make_unique<IdentifierExprNode>();
    identifier->source = result->name->source;
    result->expr = append(*result, std::move(identifier));
  }
  return result;
}

Ptr<ArgDefinitionNode> parseArgDefinition(Env* env) {
  auto result = std::make_unique<ArgDefinitionNode>();
  result->name = append(*result, parseIdentifier(env));
  if (consume(env, T::Symbol, ":")) {
    result->type = append(*result, parseType(env));
  }
  return result;
}

Ptr<ClosureExprNode> parseClosureExpr(Env* env) {
  const auto parseClosureBody = [&](Ptr<ClosureExprNode> result) {
    if (consume(env, T::Symbol, ":")) {
      result->result = append(*result, parseType(env));
    }
    require(env, "Expected: =>", T::Symbol, "=>");
    if (auto block = parseBlockStatement(env)) {
      result->blockBody = append(*result, std::move(block));
    } else {
      result->exprBody = append(*result, parseExpr(env));
    }
    return result;
  };

  const auto checkForArrow = [&](size_t i) {
    if (!ahead(env, i, T::Symbol, ")")) return false;
    return ahead(env, i + 1, T::Symbol, "=>") ||
           ahead(env, i + 1, T::Symbol, ":");
  };

  const auto checkForArgList = [&](size_t i) {
    if (checkForArrow(i)) return true;
    if (!ahead(env, i, T::Identifier)) return false;
    return ahead(env, i + 1, T::Symbol, ",") ||
           ahead(env, i + 1, T::Symbol, ":") ||
           checkForArrow(i + 1);
  };

  if (ahead(env, 0, T::Identifier) && ahead(env, 1, T::Symbol, "=>")) {
    auto result = std::make_unique<ClosureExprNode>();
    result->args.push_back(append(*result, parseArgDefinition(env)));
    return parseClosureBody(std::move(result));
  }

  if (ahead(env, 0, T::Symbol, "(") && checkForArgList(1)) {
    require(env, "Expected: (", T::Symbol, "(");
    auto result = std::make_unique<ClosureExprNode>();
    if (consume(env, T::Symbol, ")")) return parseClosureBody(std::move(result));
    do {
      result->args.push_back(append(*result, parseArgDefinition(env)));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")"));
    require(env, "Expected: )", T::Symbol, ")");
    return parseClosureBody(std::move(result));
  }
  return nullptr;
}

Ptr<ConstructorCallExprNode> parseConstructorCallExpr(Env* env) {
  if (!consume(env, T::Keyword, "new")) return nullptr;

  auto result = std::make_unique<ConstructorCallExprNode>();
  result->cls = append(*result, parseIdentifier(env));
  require(env, "Expected: (", T::Symbol, "(");
  result->args = append(*result, parseCallArgs(env));
  return result;
}

template <typename T>
Ptr<T> tokenNode(Env* env) {
    auto result = std::make_unique<T>();
    result->source = env->tokens[env->i - 1].text;
    return result;
}

Ptr<ExprNode> parseRootExpr(Env* env) {
  if (consume(env, T::Identifier)) return tokenNode<IdentifierExprNode>(env);
  if (consume(env, T::DblLiteral)) return tokenNode<DblLiteralExprNode>(env);
  if (consume(env, T::IntLiteral)) return tokenNode<IntLiteralExprNode>(env);
  if (consume(env, T::StrLiteral)) return tokenNode<StrLiteralExprNode>(env);

  if (consume(env, T::Symbol, "(")) {
    auto result = parseExpr(env);
    require(env, "Expected: )", T::Symbol, ")");
    return result;
  }

  if (consume(env, T::Symbol, "[")) {
    auto result = std::make_unique<ArrayExprNode>();
    if (consume(env, T::Symbol, "]")) return result;
    do {
      result->elements.push_back(append(*result, parseExpr(env)));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "]"));
    require(env, "Expected: ]", T::Symbol, "]");
    return result;
  }

  if (consume(env, T::Symbol, "{")) {
    auto result = std::make_unique<ObjectExprNode>();
    if (consume(env, T::Symbol, "}")) return result;
    do {
      result->items.push_back(append(*result, parseObjectItem(env)));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "}"));
    require(env, "Expected: }", T::Symbol, "}");
    return result;
  }

  if (consume(env, T::TemplateStart)) {
    auto result = std::make_unique<TemplateExprNode>();
    result->prefix = append(*result, tokenNode<TemplateNode>(env));
    while (true) {
      const size_t before = env->i;
      const auto expr = append(*result, parseExpr(env));
      if (consume(env, T::TemplateEnd)) {
        const auto text = append(*result, tokenNode<TemplateNode>(env));
        result->suffixes.push_back(TemplateExprNode::TemplatePair{expr, text});
        break;
      }
      require(env, "Expected: template", T::TemplateMid);
      const auto text = append(*result, tokenNode<TemplateNode>(env));
      result->suffixes.push_back(TemplateExprNode::TemplatePair{expr, text});
      if (env->i == before) env->i++;
    }
    return result;
  }

  env->diagnostics->push_back({cursor(env), "Expected: expression"});
  return std::make_unique<ErrorExprNode>();
}

Ptr<ExprNode> parseTermExpr(Env* env) {
  auto expr = parseRootExpr(env);
  while (true) {
    if (!check(env, T::Symbol)) break;
    const auto symbol = env->tokens[env->i].text;

    if (symbol == "(" && ++env->i) {
      auto result = std::make_unique<FunctionCallExpr>();
      result->fn   = append(*result, std::move(expr));
      result->args = append(*result, parseCallArgs(env));
      expr = std::move(result);
      continue;
    }

    if (symbol == "." && ++env->i) {
      auto result = std::make_unique<FieldAccessExprNode>();
      result->base  = append(*result, std::move(expr));
      result->field = append(*result, parseIdentifier(env));
      expr = std::move(result);
      continue;
    }

    if (symbol == "[" && ++env->i) {
      auto result = std::make_unique<IndexAccessExprNode>();
      result->base  = append(*result, std::move(expr));
      result->index = append(*result, parseExpr(env));
      require(env, "Expected: ]", T::Symbol, "]");
      expr = std::move(result);
      continue;
    }
    break;
  }
  return expr;
}

Ptr<ExprNode> parseUnaryOpExpr(Env* env) {
  std::vector<Ptr<OperatorNode>> pre;
  const auto& preops  = ops::preops();
  const auto& postops = ops::postops();

  while (check(env, T::Symbol)) {
    const auto symbol = ops::key(env->tokens[env->i].text);
    if (preops.find(symbol) == preops.end()) break;
    pre.push_back(parseOperator(env));
  }

  auto expr = parseTermExpr(env);

  while (check(env, T::Symbol)) {
    const auto symbol = ops::key(env->tokens[env->i].text);
    if (postops.find(symbol) == postops.end()) break;
    auto result = std::make_unique<UnarySuffixOpExprNode>();
    result->expr = append(*result, std::move(expr));
    result->op   = append(*result, parseOperator(env));
    expr = std::move(result);
  }

  while (!pre.empty()) {
    auto result = std::make_unique<UnaryPrefixOpExprNode>();
    result->op   = append(*result, std::move(pre.back()));
    result->expr = append(*result, std::move(expr));
    expr = std::move(result);
    pre.pop_back();
  }
  return expr;
}

Ptr<ExprNode> parseBinaryOpExpr(Env* env) {
  using Op = std::pair<Ptr<OperatorNode>, ops::Precedence>;
  std::vector<Ptr<ExprNode>> terms;
  std::vector<Op> ops;
  terms.push_back(parseUnaryOpExpr(env));
  const auto& binops = ops::binops();

  const auto evalOneOp = [&]{
    auto result = std::make_unique<BinaryOpExprNode>();
    result->lhs = append(*result, std::move(terms[terms.size() - 2]));
    result->op  = append(*result, std::move(ops.back().first));
    result->rhs = append(*result, std::move(terms[terms.size() - 1]));

    ops.pop_back();
    terms.pop_back();
    terms.back() = std::move(result);
  };

  while (true) {
    if (!check(env, T::Symbol)) break;
    const auto pos = cursor(env);
    const auto symbol = ops::key(env->tokens[env->i].text);
    const auto it = binops.find(symbol);
    if (it == binops.end()) break;

    auto op = Op{parseOperator(env), it->second};
    while (!ops.empty() && ops.back().second.glb <= op.second.lub) {
      const auto& prev = ops.back();
      const auto same = prev.first->source == op.first->source;
      if (same || prev.second.glb == op.second.lub) {
        if (!op.second.repeat) {
          std::stringstream ss;
          ss << "Non-associative op: " << op.first->source;
          env->diagnostics->push_back({pos, ss.str()});
        }
      } else if (op.second.glb <= prev.second.lub) {
        std::stringstream ss;
        ss << "Ambiguous precedence: " << ops.back().first->source
           << " vs. " << op.first->source;
        env->diagnostics->push_back({pos, ss.str()});
      }
      evalOneOp();
    }
    ops.push_back(std::move(op));
    terms.push_back(parseUnaryOpExpr(env));
  }

  while (!ops.empty()) evalOneOp();
  auto result = std::move(terms.back());
  terms.pop_back();
  return result;
}

Ptr<ExprNode> parseExpr(Env* env) {
  // NOCOMMIT
  //if (auto closure = parseClosureExpr(env)) return closure;
  if (auto ctor = parseConstructorCallExpr(env)) return ctor;

  auto lhs = parseBinaryOpExpr(env);
  if (!check(env, T::Symbol)) return lhs;

  if (consume(env, T::Symbol, "?")) {
    auto result = std::make_unique<TernaryExprNode>();
    result->cond = append(*result, std::move(lhs));
    result->lhs  = append(*result, parseExpr(env));
    require(env, "Expected: :", T::Symbol, ":");
    result->rhs  = append(*result, parseExpr(env));
    return result;
  }

  const auto symbol = ops::key(env->tokens[env->i].text);
  const auto& assignment = ops::assignment();
  if (assignment.find(symbol) != assignment.end()) {
    auto result = std::make_unique<AssignmentExprNode>();
    result->lhs = append(*result, std::move(lhs));
    result->op  = append(*result, parseOperator(env));
    result->rhs = append(*result, parseExpr(env));
    return result;
  }
  return lhs;
}

// Statement grammar.

Ptr<StatementNode> parseTrivialStatement(
    Env* env, const char* message, const char* text) {
  if (consume(env, T::Symbol, text)) {
    return std::make_unique<EmptyStatementNode>();
  }
  auto result = std::make_unique<ExprStatementNode>();
  result->expr = append(*result, parseExpr(env));
  require(env, message, T::Symbol, text);
  return result;
}

Ptr<StatementNode> parseTrivialStatementAndColon(Env* env) {
  return parseTrivialStatement(env, "Expected: ;", ";");
}

Ptr<StatementNode> parseTrivialStatementAndParen(Env* env) {
  return parseTrivialStatement(env, "Expected: )", ")");
}

Ptr<BlockStatementNode> parseBlockStatement(Env* env) {
  if (!consume(env, T::Symbol, "{")) return nullptr;
  auto result = std::make_unique<BlockStatementNode>();
  while (!consume(env, T::Symbol, "}")) {
    const size_t before = env->i;
    result->statements.push_back(append(*result, parseStatement(env)));
    if (env->i == before) env->i++;
  }
  return result;
}

void parseClassComponent(Env* env, ClassDeclarationStatementNode& cls) {
  auto access = std::unique_ptr<KeywordNode>();
  if (check(env, T::Keyword, "private")) access = parseKeyword(env);
  auto name = parseIdentifier(env);

  if (consume(env, T::Symbol, "(")) {
    auto result = std::make_unique<ClassMethodNode>();
    result->access = append(*result, std::move(access));
    result->name = append(*result, std::move(name));
    if (!consume(env, T::Symbol, ")")) {
      do {
        result->args.push_back(append(*result, parseArgDefinition(env)));
      } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")"));
      require(env, "Expected: )", T::Symbol, ")");
    }
    if (consume(env, T::Symbol, ":")) {
      result->result = append(*result, parseType(env));
    }
    if (auto block = parseBlockStatement(env)) {
      result->blockBody = append(*result, std::move(block));
    } else {
      result->exprBody = append(*result, parseExpr(env));
    }
    cls.methods.push_back(append(cls, std::move(result)));
    return;
  }

  auto result = std::make_unique<ClassMemberNode>();
  result->access = append(*result, std::move(access));
  result->name = append(*result, std::move(name));
  if (consume(env, T::Symbol, ":")) {
    result->type = append(*result, parseType(env));
  }
  if (consume(env, T::Symbol, "=")) {
    result->init = append(*result, parseExpr(env));
  }
  require(env, "Expected: ;", T::Symbol, ";");
  cls.members.push_back(append(cls, std::move(result)));
}

Ptr<ClassDeclarationStatementNode> parseClassDeclaration(Env* env) {
  auto result = std::make_unique<ClassDeclarationStatementNode>();
  require(env, "Expected: class", T::Keyword, "class");
  result->name = append(*result, parseIdentifier(env));
  if (consume(env, T::Keyword, "extends")) {
    result->base = append(*result, parseIdentifier(env));
  }
  require(env, "Expected: {", T::Symbol, "{");
  while (!consume(env, T::Symbol, "}")) {
    const size_t before = env->i;
    parseClassComponent(env, *result);
    if (env->i == before) env->i++;
  }
  require(env, "Expected: ;", T::Symbol, ";");
  result->kind = SK::ClassDeclarationStatement;
  return result;
}

Ptr<StatementNode> parseStatement(Env* env) {
  const auto parseDeclaration = [&]() -> Ptr<StatementNode> {
    auto result = std::make_unique<DeclarationStatementNode>();
    result->keyword = append(*result, parseKeyword(env));
    result->root = append(*result, parseRootExpr(env));
    if (consume(env, T::Symbol, ":")) {
      result->type = append(*result, parseType(env));
    }
    require(env, "Expected: =", T::Symbol, "=");
    result->expr = append(*result, parseExpr(env));
    require(env, "Expected: ;", T::Symbol, ";");
    return result;
  };

  const auto parseIfStatement = [&]() -> Ptr<StatementNode> {
    env->i++;
    auto result = std::make_unique<IfStatementNode>();
    const auto parseCond = [&]{
      require(env, "Expected: (", T::Symbol, "(");
      const auto cond = append(*result, parseExpr(env));
      require(env, "Expected: )", T::Symbol, ")");
      const auto then = append(*result, parseStatement(env));
      result->cases.push_back(IfStatementNode::CondClause{cond, then});
    };
    parseCond();
    while (!result->elseCase && consume(env, T::Keyword, "else")) {
      if (consume(env, T::Keyword, "if")) {
        parseCond();
      } else {
        result->elseCase = append(*result, parseStatement(env));
      }
    }
    return result;
  };

  const auto parseForLoop = [&]() -> Ptr<StatementNode> {
    env->i++;
    auto result = std::unique_ptr<ForLoopStatementNode>();
    require(env, "Expected: (", T::Symbol, "(");

    if (check(env, T::Keyword, "const") || check(env, T::Keyword, "let")) {
      auto keyword = parseKeyword(env);
      auto root = parseRootExpr(env);
      auto type = std::unique_ptr<TypeNode>();

      if (consume(env, T::Symbol, ":")) type = parseType(env);

      if (consume(env, T::Keyword, "of")) {
        auto result = std::make_unique<ForEachStatementNode>();
        result->init.keyword = append(*result, std::move(keyword));
        result->init.root = append(*result, std::move(root));
        if (type) result->init.type = append(*result, std::move(type));
        result->init.expr = append(*result, parseExpr(env));
        require(env, "Expected: )", T::Symbol, ")");
        result->body = append(*result, parseStatement(env));
        return result;
      }

      require(env, "Expected: =", T::Symbol, "=");
      auto init = std::make_unique<DeclarationStatementNode>();
      init->keyword = append(*result, std::move(keyword));
      init->root = append(*result, std::move(root));
      if (type) init->type = append(*result, std::move(type));
      init->expr = append(*result, parseExpr(env));
      require(env, "Expected: ;", T::Symbol, ";");
      result->init = append(*result, std::move(init));
    } else {
      result->init = append(*result, parseTrivialStatementAndColon(env));
    }

    result->cond = append(*result, parseExpr(env));
    require(env, "Expected: ;", T::Symbol, ";");
    result->post = append(*result, parseTrivialStatementAndParen(env));
    result->body = append(*result, parseStatement(env));
    return result;
  };

  const auto parseWhileLoop = [&]() -> Ptr<StatementNode> {
    env->i++;
    auto result = std::make_unique<WhileLoopStatementNode>();
    require(env, "Expected: (", T::Symbol, "(");
    result->cond = append(*result, parseExpr(env));
    require(env, "Expected: )", T::Symbol, ")");
    result->body = append(*result, parseStatement(env));
    return result;
  };

  const auto parseReturn = [&]() -> Ptr<StatementNode> {
    env->i++;
    auto result = std::make_unique<ReturnStatementNode>();
    if (!consume(env, T::Symbol, ";")) {
      result->expr = append(*result, parseExpr(env));
      require(env, "Expected: ;", T::Symbol, ";");
    }
    return result;
  };

  const auto parseTypeAlias = [&]() -> Ptr<StatementNode> {
    env->i++;
    auto result = std::make_unique<TypeAliasStatementNode>();
    result->lhs = append(*result, parseIdentifier(env));
    require(env, "Expected: =", T::Symbol, "=");
    result->rhs = append(*result, parseType(env));
    require(env, "Expected: ;", T::Symbol, ";");
    return result;
  };

  if (check(env, T::Keyword)) {
    const auto keyword = env->tokens[env->i].text;
    if (keyword == "const")     return parseDeclaration();
    if (keyword == "let")       return parseDeclaration();
    if (keyword == "if")        return parseIfStatement();
    if (keyword == "for")       return parseForLoop();
    if (keyword == "while")     return parseWhileLoop();
    if (keyword == "return")    return parseReturn();
    if (keyword == "break")     return tokenNode<BreakStatementNode>(env);
    if (keyword == "continue")  return tokenNode<ContinueStatementNode>(env);
    if (keyword == "class")     return parseClassDeclaration(env);
    if (keyword == "type")      return parseTypeAlias();
  }

  if (auto block = parseBlockStatement(env)) return block;
  return parseTrivialStatementAndColon(env);
}

Ptr<ProgramNode> parseProgram(Env* env) {
  auto result = std::make_unique<ProgramNode>();
  while (env->i < env->tokens.size()) {
    const size_t before = env->i;
    result->statements.push_back(append(*result, parseStatement(env)));
    if (env->i == before) env->i++;
  }
  return result;
}

Ptr<Node> parse(const std::string& input,
                const std::vector<lexer::Token>& tokens,
                std::vector<Diagnostic>* diagnostics) {
  parser::Env env{input, tokens, diagnostics, 0};
  return parser::parseProgram(&env);
}

} // namespace parser

// Entry point.

std::string formatTokens(const std::vector<lexer::Token>& tokens) {
  std::stringstream ss;
  for (const auto& x : tokens) {
    ss << desc(x.type) << ": " << x.text << "\n";
  }
  return ss.str();
}

std::string formatAST(const ast::Node& node) {
  std::stringstream ss;

  const std::function<void(const ast::Node&, size_t)> recurse =
      [&](const auto& node, auto depth) {
    const std::string spacer = std::string(2 * depth, ' ');
    ss << spacer << node.describe();
    if (node.children.empty() && node.source.data()) ss << ": " << node.source;
    ss << '\n';
    for (const auto& child : node.children) recurse(*child, depth + 1);
  };
  recurse(node, 0);

  return ss.str();
}

std::string formatDiagnostics(
    const std::string& input, std::vector<Diagnostic>* diagnostics) {
  const size_t size = input.size();
  std::stable_sort(diagnostics->begin(), diagnostics->end(),
                   [](const auto& a, const auto& b) { return a.pos < b.pos; });
  std::pair<size_t, size_t> cur{-1, -1};
  size_t line = 0;

  std::stringstream ss;
  size_t prev = static_cast<size_t>(-1);
  for (const auto& diagnostic : *diagnostics) {
    const size_t pos = std::min(diagnostic.pos, size);
    if (pos == prev) continue;
    while ((cur.second + 1) < (pos + 1)) {
      line++;
      cur.first = cur.second;
      size_t& next = cur.second;
      for (next++; next < size && input[next] != '\n'; next++) {}
    }
    ss << line << ':' << (pos - cur.first) << ':' << diagnostic.error << '\n';
    prev = pos;
  }
  return ss.str();
}

int main(int argc, const char** argv) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " $FILE" << std::endl;
    return 1;
  }

  std::ifstream is(argv[1]);
  std::stringstream ss;
  while (is >> ss.rdbuf());
  const auto input = ss.str();

  std::vector<Diagnostic> diagnostics;
  const auto tokens = lexer::lex(input, &diagnostics);
  //std::cout << formatTokens(tokens);
  const auto program = parser::parse(input, tokens, &diagnostics);
  std::cerr << formatAST(*program);
  std::cerr << std::endl;
  std::cerr << formatDiagnostics(input, &diagnostics);
  return diagnostics.empty() ? 0 : 1;
}
