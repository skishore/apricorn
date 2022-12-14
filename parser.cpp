#include <array>
#include <cstdint>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

struct Diagnostic {
  size_t pos;
  std::string error;
};

// string_view, since we are targeting C++ 14.

struct string_view {
  string_view() : m_data(nullptr), m_size(0) {}
  string_view(const char* data) : m_data(data), m_size(strlen(data)) {}
  string_view(const char* data, size_t size) : m_data(data), m_size(size) {}

  const char* data() const { return m_data; }
  size_t size() const { return m_size; }

  const char* m_data;
  size_t m_size;
};

bool operator==(const string_view& a, const string_view& b) {
  if (a.size() != b.size()) return false;
  return memcmp(a.data(), b.data(), b.size()) == 0;
}

std::ostream& operator<<(std::ostream& os, const string_view& sv) {
  if (sv.size()) os.write(sv.data(), static_cast<ssize_t>(sv.size()));
  return os;
}

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
    "extends", "for", "if", "import", "interface", "let", "new",
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

const char* tokenTypeName(TokenType type) {
  switch (type) {
#define X(name) case TokenType::name: return #name;
  TOKEN_TYPE
#undef X
  }
};

#undef TOKEN_TYPE

struct Token {
  TokenType type;
  string_view text;
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

#define NODE_TYPES          \
  X(Program)                \
  /* Type nodes. */         \
  X(BaseType)               \
  X(ArrayType)              \
  X(TupleType)              \
  X(UnionType)              \
  X(ObjectType)             \
  X(GenericType)            \
  X(ClosureType)            \
  /* OOP nodes. */          \
  X(Class)                  \
  X(Member)                 \
  X(Method)                 \
  X(Extends)                \
  /* Statement nodes. */    \
  X(IfStatement)            \
  X(ExprStatement)          \
  X(BlockStatement)         \
  X(ForLoopStatement)       \
  X(ForEachLoopStatement)   \
  X(WhileLoopStatement)     \
  X(ReturnStatement)        \
  X(ControlStatement)       \
  X(DeclarationStatement)   \
  X(ExportStatement)        \
  X(ImportStatement)        \
  X(InterfaceStatement)     \
  X(TypeAliasStatement)     \
  /* Expression nodes. */   \
  X(BinOpExpr)              \
  X(UnaryOpExpr)            \
  X(ArrayExpr)              \
  X(ObjectExpr)             \
  X(ClosureExpr)            \
  X(TernaryExpr)            \
  X(TemplateExpr)           \
  X(AssignmentExpr)         \
  X(IdentifierExpr)         \
  X(DblLiteralExpr)         \
  X(IntLiteralExpr)         \
  X(StrLiteralExpr)         \
  X(FieldAccessExpr)        \
  X(IndexAccessExpr)        \
  X(FunctionCallExpr)       \
  X(ConstructorCallExpr)    \
  /* Auxiliary nodes. */    \
  X(Generic)                \
  X(Keyword)                \
  X(Operator)               \
  X(CallArgs)               \
  X(Identifier)             \
  X(ArgDefinition)          \
  X(ArgDefinitions)         \
  X(TemplateString)         \
  X(ObjectItem)             \
  X(CondClause)             \
  X(ElseClause)             \
  X(ForLoopInitializer)     \
  X(ForLoopCondition)       \
  X(ForLoopIncrement)       \
  X(ForEachLoopInitializer) \
  /* Error node. */         \
  X(Error)                  \

enum class NodeType : uint8_t {
#define X(name) name,
  NODE_TYPES
#undef X
};

const char* nodeTypeName(NodeType type) {
  switch (type) {
#define X(name) case NodeType::name: return #name;
  NODE_TYPES
#undef X
  }
};

#undef NODE_TYPES

struct Node {
  Node() {}
  Node(const Node& o) = delete;
  Node& operator=(const Node& o) = delete;
  Node(Node&& o) = default;
  Node& operator=(Node&& o) = default;

  NodeType type = NodeType::Error;
  std::vector<Ptr<Node>> children;
  string_view source;
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

Symbol key(const string_view& symbol) {
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
using N = NodeType;

struct Env {
  const std::string& input;
  const std::vector<Token>& tokens;
  std::vector<Diagnostic>* diagnostics;
  size_t i;
};

size_t cursor(Env* env) {
  return env->i < env->input.size()
    ? static_cast<size_t>(env->tokens[env->i].text.data() - env->input.data())
    : env->input.size();
}

string_view source(Env* env, size_t pos, size_t end) {
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

Ptr<Node> parseToken(Env* env, TokenType tt, NodeType nt, const char* error) {
  auto result = std::make_unique<Node>();
  if (require(env, error, tt)) {
    result->source = env->tokens[env->i - 1].text;
    result->type = nt;
  }
  return result;
}

Ptr<Node> parsePath(Env* env) {
  return parseToken(env, T::StrLiteral, N::StrLiteralExpr, "Expected: path");
}

Ptr<Node> parseIdentifier(Env* env) {
  return parseToken(env, T::Identifier, N::Identifier, "Expected: identifier");
}

Ptr<Node> parseKeyword(Env* env) {
  return parseToken(env, T::Keyword, N::Keyword, "Expected: keyword");
}

Ptr<Node> parseOperator(Env* env) {
  return parseToken(env, T::Symbol, N::Operator, "Expected: operator");
}

Ptr<Node> parseBaseType(Env* env) {
  return parseToken(env, T::Identifier, N::BaseType, "Expected: type");
}

Ptr<Node> parseGeneric(Env* env) {
  return parseToken(env, T::Identifier, N::Generic, "Expected: generic");
}

// Type grammar.

Ptr<Node> parseType(Env* env);

Ptr<Node> parseClosureArgType(Env* env) {
  auto result = std::make_unique<Node>();
  result->children.push_back(parseIdentifier(env));
  require(env, "Expected: :", T::Symbol, ":");
  result->children.push_back(parseType(env));
  result->type = N::ArgDefinition;
  return result;
}

Ptr<Node> parseClosureType(Env* env) {
  const auto parseClosureBody = [&](Ptr<Node> args) {
    auto result = std::make_unique<Node>();
    result->children.push_back(std::move(args));
    require(env, "Expected: =>", T::Symbol, "=>");
    result->children.push_back(parseType(env));
    result->type = N::ClosureType;
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
    auto args = std::make_unique<Node>();
    args->type = N::ArgDefinitions;
    if (consume(env, T::Symbol, ")")) return parseClosureBody(std::move(args));
    args->children.push_back(parseClosureArgType(env));
    while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")")) {
      args->children.push_back(parseClosureArgType(env));
    }
    require(env, "Expected: )", T::Symbol, ")");
    return parseClosureBody(std::move(args));
  }

  return nullptr;
}

Ptr<Node> parseDictTypeItem(Env* env) {
  auto result = std::make_unique<Node>();
  result->children.push_back(parseIdentifier(env));
  require(env, "Expected: :", T::Symbol, ":");
  result->children.push_back(parseType(env));
  result->type = N::ObjectItem;
  return result;
}

Ptr<Node> parseQualifiedType(Env* env, bool lhs) {
  auto result = parseBaseType(env);
  if (!consume(env, T::Symbol, "<")) return result;

  auto generic = std::make_unique<Node>();
  generic->children.push_back(std::move(result));
  const std::function<Ptr<Node>(Env*)> fn = lhs ? parseGeneric : parseType;
  generic->children.push_back(fn(env));
  while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ">")) {
    generic->children.push_back(fn(env));
  }
  require(env, "Expected: >", T::Symbol, ">");
  generic->type = N::GenericType;
  return generic;
}

Ptr<Node> parseRootType(Env* env) {
  if (auto closure = parseClosureType(env)) return closure;

  if (consume(env, T::Symbol, "(")) {
    auto result = parseType(env);
    require(env, "Expected: )", T::Symbol, ")");
    return result;
  }

  if (consume(env, T::Symbol, "[")) {
    auto result = std::make_unique<Node>();
    result->type = N::TupleType;
    if (consume(env, T::Symbol, "]")) return result;
    result->children.push_back(parseType(env));
    while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "]")) {
      result->children.push_back(parseType(env));
    }
    require(env, "Expected: ]", T::Symbol, "]");
    return result;
  }

  if (consume(env, T::Symbol, "{")) {
    auto result = std::make_unique<Node>();
    result->type = N::ObjectType;
    if (consume(env, T::Symbol, "}")) return result;
    result->children.push_back(parseDictTypeItem(env));
    while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "}")) {
      result->children.push_back(parseDictTypeItem(env));
    }
    require(env, "Expected: }", T::Symbol, "}");
    return result;
  }

  return parseQualifiedType(env, false);
}

Ptr<Node> parseTermType(Env* env) {
  auto result = parseRootType(env);
  if (consume(env, T::Symbol, "[")) {
    require(env, "Expected: ]", T::Symbol, "]");
    auto list = std::make_unique<Node>();
    list->children.push_back(std::move(result));
    list->type = N::ArrayType;
    result = std::move(list);
  }
  return result;
}

Ptr<Node> parseType(Env* env) {
  auto result = parseTermType(env);
  if (!consume(env, T::Symbol, "|")) return result;

  auto ut = std::make_unique<Node>();
  ut->children.push_back(std::move(result));
  ut->children.push_back(parseTermType(env));
  while (consume(env, T::Symbol, "|")) {
    ut->children.push_back(parseTermType(env));
  }
  ut->type = N::UnionType;
  return ut;
}

// Expression grammar.

Ptr<Node> parseExpr(Env* env);
Ptr<Node> parseStatement(Env* env);
Ptr<Node> parseBlockStatement(Env* env);

Ptr<Node> parseCallArgs(Env* env) {
  auto result = std::make_unique<Node>();
  result->type = N::CallArgs;
  if (consume(env, T::Symbol, ")")) return result;
  result->children.push_back(parseExpr(env));
  while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")")) {
    result->children.push_back(parseExpr(env));
  }
  require(env, "Expected: )", T::Symbol, ")");
  return result;
}

Ptr<Node> parseDictItem(Env* env) {
  auto result = std::make_unique<Node>();
  result->children.push_back(parseIdentifier(env));
  if (consume(env, T::Symbol, ":")) {
    result->children.push_back(parseExpr(env));
  }
  result->type = N::ObjectItem;
  return result;
}

Ptr<Node> parseArgDefinition(Env* env) {
  auto result = std::make_unique<Node>();
  result->children.push_back(parseIdentifier(env));
  if (consume(env, T::Symbol, ":")) {
    result->children.push_back(parseType(env));
  }
  result->type = N::ArgDefinition;
  return result;
}

Ptr<Node> parseClosure(Env* env) {
  const auto parseClosureBody = [&](Ptr<Node> args) {
    auto result = std::make_unique<Node>();
    result->children.push_back(std::move(args));
    if (consume(env, T::Symbol, ":")) {
      result->children.push_back(parseType(env));
    }
    require(env, "Expected: =>", T::Symbol, "=>");
    if (auto block = parseBlockStatement(env)) {
      result->children.push_back(std::move(block));
    } else {
      auto expr = std::make_unique<Node>();
      expr->children.push_back(parseExpr(env));
      expr->type = NodeType::ExprStatement;
      result->children.push_back(std::move(expr));
    }
    result->type = N::ClosureExpr;
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
    auto args = std::make_unique<Node>();
    args->children.push_back(parseArgDefinition(env));
    args->type = N::ArgDefinitions;
    return parseClosureBody(std::move(args));
  }

  if (ahead(env, 0, T::Symbol, "(") && checkForArgList(1)) {
    require(env, "Expected: (", T::Symbol, "(");
    auto args = std::make_unique<Node>();
    args->type = N::ArgDefinitions;
    if (consume(env, T::Symbol, ")")) return parseClosureBody(std::move(args));
    args->children.push_back(parseArgDefinition(env));
    while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")")) {
      args->children.push_back(parseArgDefinition(env));
    }
    require(env, "Expected: )", T::Symbol, ")");
    return parseClosureBody(std::move(args));
  }
  return nullptr;
}

Ptr<Node> parseConstructorCall(Env* env) {
  if (!consume(env, T::Keyword, "new")) return nullptr;

  auto result = std::make_unique<Node>();
  result->children.push_back(parseIdentifier(env));
  require(env, "Expected: (", T::Symbol, "(");
  result->children.push_back(parseCallArgs(env));
  result->type = N::ConstructorCallExpr;
  return result;
}

Ptr<Node> parseRootExpr(Env* env) {
  const auto token = [&](NodeType type) {
    auto result = std::make_unique<Node>();
    result->source = env->tokens[env->i - 1].text;
    result->type = type;
    return result;
  };
  if (consume(env, T::Identifier)) return token(N::IdentifierExpr);
  if (consume(env, T::DblLiteral)) return token(N::DblLiteralExpr);
  if (consume(env, T::IntLiteral)) return token(N::IntLiteralExpr);
  if (consume(env, T::StrLiteral)) return token(N::StrLiteralExpr);

  if (consume(env, T::Symbol, "(")) {
    auto result = parseExpr(env);
    require(env, "Expected: )", T::Symbol, ")");
    return result;
  }

  if (consume(env, T::Symbol, "[")) {
    auto result = std::make_unique<Node>();
    result->type = N::ArrayExpr;
    if (consume(env, T::Symbol, "]")) return result;
    result->children.push_back(parseExpr(env));
    while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "]")) {
      result->children.push_back(parseExpr(env));
    }
    require(env, "Expected: ]", T::Symbol, "]");
    return result;
  }

  if (consume(env, T::Symbol, "{")) {
    auto result = std::make_unique<Node>();
    result->type = N::ObjectExpr;
    if (consume(env, T::Symbol, "}")) return result;
    result->children.push_back(parseDictItem(env));
    while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "}")) {
      result->children.push_back(parseDictItem(env));
    }
    require(env, "Expected: }", T::Symbol, "}");
    return result;
  }

  if (consume(env, T::TemplateStart)) {
    auto result = std::make_unique<Node>();
    result->children.push_back(token(N::TemplateString));
    while (true) {
      const size_t before = env->i;
      result->children.push_back(parseExpr(env));
      if (consume(env, T::TemplateEnd)) {
        result->children.push_back(token(N::TemplateString));
        break;
      }
      require(env, "Expected: template", T::TemplateMid);
      result->children.push_back(token(N::TemplateString));
      if (env->i == before) env->i++;
    }
    result->type = N::TemplateExpr;
    return result;
  }

  env->diagnostics->push_back({cursor(env), "Expected: expression"});
  return std::make_unique<Node>();
}

Ptr<Node> parseTermExpr(Env* env) {
  auto expr = parseRootExpr(env);
  while (true) {
    if (!check(env, T::Symbol)) break;
    const auto symbol = env->tokens[env->i].text;

    if (symbol == "(" && ++env->i) {
      auto result = std::make_unique<Node>();
      result->children.push_back(std::move(expr));
      result->children.push_back(parseCallArgs(env));
      result->type = N::FunctionCallExpr;
      expr = std::move(result);
      continue;
    }

    if (symbol == "." && ++env->i) {
      auto result = std::make_unique<Node>();
      result->children.push_back(std::move(expr));
      result->children.push_back(parseIdentifier(env));
      result->type = N::FieldAccessExpr;
      expr = std::move(result);
      continue;
    }

    if (symbol == "[" && ++env->i) {
      auto result = std::make_unique<Node>();
      result->children.push_back(std::move(expr));
      result->children.push_back(parseExpr(env));
      require(env, "Expected: ]", T::Symbol, "]");
      result->type = N::IndexAccessExpr;
      expr = std::move(result);
      continue;
    }
    break;
  }
  return expr;
}

Ptr<Node> parseUnaryOpExpr(Env* env) {
  std::vector<Ptr<Node>> pre;
  const auto& preops  = ops::preops();
  const auto& postops = ops::postops();

  const auto unary = [&](Ptr<Node> a, Ptr<Node> b) {
    auto result = std::make_unique<Node>();
    result->children.push_back(std::move(a));
    result->children.push_back(std::move(b));
    result->type = N::UnaryOpExpr;
    return result;
  };

  while (check(env, T::Symbol)) {
    const auto symbol = ops::key(env->tokens[env->i].text);
    if (preops.find(symbol) == preops.end()) break;
    pre.push_back(parseOperator(env));
  }

  auto expr = parseTermExpr(env);

  while (check(env, T::Symbol)) {
    const auto symbol = ops::key(env->tokens[env->i].text);
    if (postops.find(symbol) == postops.end()) break;
    expr = unary(std::move(expr), parseOperator(env));
  }

  while (!pre.empty()) {
    expr = unary(std::move(pre.back()), std::move(expr));
    pre.pop_back();
  }
  return expr;
}

Ptr<Node> parseBinOpExpr(Env* env) {
  using Op = std::pair<Ptr<Node>, ops::Precedence>;
  std::vector<Ptr<Node>> terms;
  std::vector<Op> ops;
  terms.push_back(parseUnaryOpExpr(env));
  const auto& binops = ops::binops();

  const auto evalOneOp = [&]{
    auto result = std::make_unique<Node>();
    result->children.push_back(std::move(terms[terms.size() - 2]));
    result->children.push_back(std::move(ops.back().first));
    result->children.push_back(std::move(terms[terms.size() - 1]));
    result->type = N::BinOpExpr;

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

Ptr<Node> parseExpr(Env* env) {
  if (auto closure = parseClosure(env)) return closure;
  if (auto ctor = parseConstructorCall(env)) return ctor;

  auto lhs = parseBinOpExpr(env);
  if (!check(env, T::Symbol)) return lhs;

  if (consume(env, T::Symbol, "?")) {
    auto result = std::make_unique<Node>();
    result->children.push_back(std::move(lhs));
    result->children.push_back(parseExpr(env));
    require(env, "Expected: :", T::Symbol, ":");
    result->children.push_back(parseExpr(env));
    result->type = N::TernaryExpr;
    return result;
  }

  const auto symbol = ops::key(env->tokens[env->i].text);
  const auto& assignment = ops::assignment();
  if (assignment.find(symbol) != assignment.end()) {
    auto result = std::make_unique<Node>();
    result->children.push_back(std::move(lhs));
    result->children.push_back(parseOperator(env));
    result->children.push_back(parseExpr(env));
    result->type = N::AssignmentExpr;
    return result;
  }
  return lhs;
}

// Statement grammar.

Ptr<Node> parseExprStatement(Env* env) {
  auto result = std::make_unique<Node>();
  result->children.push_back(parseExpr(env));
  require(env, "Expected: ;", T::Symbol, ";");
  result->type = NodeType::ExprStatement;
  return result;
}

Ptr<Node> parseBlockStatement(Env* env) {
  if (!consume(env, T::Symbol, "{")) return nullptr;
  auto result = std::make_unique<Node>();
  while (!consume(env, T::Symbol, "}")) {
    const size_t before = env->i;
    result->children.push_back(parseStatement(env));
    if (env->i == before) env->i++;
  }
  result->type = N::BlockStatement;
  return result;
}

Ptr<Node> parseClassComponent(Env* env) {
  auto result = std::make_unique<Node>();
  result->children.push_back(parseIdentifier(env));

  if (consume(env, T::Symbol, "(")) {
    auto args = std::make_unique<Node>();
    args->type = N::ArgDefinitions;
    if (!consume(env, T::Symbol, ")")) {
      args->children.push_back(parseArgDefinition(env));
      while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")")) {
        args->children.push_back(parseArgDefinition(env));
      }
      require(env, "Expected: )", T::Symbol, ")");
    }
    result->children.push_back(std::move(args));
    if (consume(env, T::Symbol, ":")) {
      result->children.push_back(parseType(env));
    }
    if (auto block = parseBlockStatement(env)) {
      result->children.push_back(std::move(block));
    } else {
      result->children.push_back(std::make_unique<Node>());
    }
    result->type = N::Method;
    return result;
  }

  if (consume(env, T::Symbol, ":")) {
    result->children.push_back(parseType(env));
  }
  if (consume(env, T::Symbol, "=")) {
    result->children.push_back(parseExpr(env));
  }
  require(env, "Expected: ;", T::Symbol, ";");
  result->type = N::Member;
  return result;
}

Ptr<Node> parseClassDeclaration(Env* env) {
  auto result = std::make_unique<Node>();
  require(env, "Expected: class", T::Keyword, "class");
  result->children.push_back(parseIdentifier(env));
  if (consume(env, T::Keyword, "extends")) {
    auto extends = std::make_unique<Node>();
    extends->children.push_back(parseType(env));
    extends->type = N::Extends;
    result->children.push_back(std::move(extends));
  }
  require(env, "Expected: {", T::Symbol, "{");
  while (!consume(env, T::Symbol, "}")) {
    const size_t before = env->i;
    result->children.push_back(parseClassComponent(env));
    if (env->i == before) env->i++;
  }
  require(env, "Expected: ;", T::Symbol, ";");
  result->type = N::Class;
  return result;
}

Ptr<Node> parseStatement(Env* env) {
  const auto parseControl = [&]() -> Ptr<Node> {
    auto result = std::make_unique<Node>();
    result->children.push_back(parseKeyword(env));
    require(env, "Expected: ;", T::Symbol, ";");
    result->type = N::ControlStatement;
    return result;
  };

  const auto parseDeclaration = [&]() -> Ptr<Node> {
    auto result = std::make_unique<Node>();
    result->children.push_back(parseKeyword(env));
    result->children.push_back(parseRootExpr(env));
    if (consume(env, T::Symbol, ":")) {
      result->children.push_back(parseType(env));
    }
    require(env, "Expected: =", T::Symbol, "=");
    result->children.push_back(parseExpr(env));
    require(env, "Expected: ;", T::Symbol, ";");
    result->type = N::DeclarationStatement;
    return result;
  };

  const auto parseIfClause = [&](NodeType type) -> Ptr<Node> {
    auto result = std::make_unique<Node>();
    if (type == N::CondClause) {
      require(env, "Expected: (", T::Symbol, "(");
      result->children.push_back(parseExpr(env));
      require(env, "Expected: )", T::Symbol, ")");
    }
    result->children.push_back(parseStatement(env));
    result->type = type;
    return result;
  };

  const auto parseIfStatement = [&]() -> Ptr<Node> {
    env->i++;
    auto result = std::make_unique<Node>();
    result->children.push_back(parseIfClause(N::CondClause));
    while (result->children.back()->type != N::ElseClause &&
           consume(env, T::Keyword, "else")) {
      if (consume(env, T::Keyword, "if")) {
        result->children.push_back(parseIfClause(N::CondClause));
      } else {
        result->children.push_back(parseIfClause(N::ElseClause));
      }
    }
    result->type = N::IfStatement;
    return result;
  };

  const auto parseForLoopHeader = [&](NodeType type) -> Ptr<Node> {
    auto header = std::make_unique<Node>();
    header->type = type;
    const char* terminator =
      type == N::ForLoopIncrement ? ")" : ";";
    const char* expected_terminator =
      type == N::ForLoopIncrement ? "Expected: )" : "Expected: ;";
    if (consume(env, T::Symbol, terminator)) return header;
    header->children.push_back(parseExpr(env));
    while (consume(env, T::Symbol, ",")) {
      header->children.push_back(parseExpr(env));
    }
    require(env, expected_terminator, T::Symbol, terminator);
    return header;
  };

  const auto parseForLoop = [&]() -> Ptr<Node> {
    env->i++;
    auto result = std::make_unique<Node>();
    require(env, "Expected: (", T::Symbol, "(");

    if (check(env, T::Keyword, "const") || check(env, T::Keyword, "let")) {
      auto initializer = std::make_unique<Node>();
      initializer->children.push_back(parseKeyword(env));
      initializer->children.push_back(parseRootExpr(env));

      if (consume(env, T::Keyword, "of")) {
        initializer->children.push_back(parseExpr(env));
        initializer->type = N::ForEachLoopInitializer;
        require(env, "Expected: )", T::Symbol, ")");
        result->children.push_back(std::move(initializer));
        result->children.push_back(parseStatement(env));
        result->type = N::ForEachLoopStatement;
        return result;
      }

      require(env, "Expected: =", T::Symbol, "=");
      initializer->children.push_back(parseExpr(env));
      require(env, "Expected: ;", T::Symbol, ";");
      initializer->type = N::DeclarationStatement;
      result->children.push_back(std::move(initializer));
    } else {
      result->children.push_back(parseForLoopHeader(N::ForLoopInitializer));
    }

    result->children.push_back(parseForLoopHeader(N::ForLoopCondition));
    result->children.push_back(parseForLoopHeader(N::ForLoopIncrement));
    result->children.push_back(parseStatement(env));
    result->type = N::ForLoopStatement;
    return result;
  };

  const auto parseWhileLoop = [&]() -> Ptr<Node> {
    env->i++;
    auto result = std::make_unique<Node>();
    require(env, "Expected: (", T::Symbol, "(");
    result->children.push_back(parseExpr(env));
    require(env, "Expected: )", T::Symbol, ")");
    result->children.push_back(parseStatement(env));
    result->type = N::WhileLoopStatement;
    return result;
  };

  const auto parseReturn = [&]() -> Ptr<Node> {
    env->i++;
    auto result = std::make_unique<Node>();
    if (!consume(env, T::Symbol, ";")) {
      result->children.push_back(parseExpr(env));
      require(env, "Expected: ;", T::Symbol, ";");
    }
    result->type = N::ReturnStatement;
    return result;
  };

  const auto parseTypeDecl = [&](NodeType type) -> Ptr<Node> {
    env->i++;
    auto result = std::make_unique<Node>();
    result->children.push_back(parseQualifiedType(env, true));
    if (type == N::TypeAliasStatement) {
      require(env, "Expected: =", T::Symbol, "=");
    }
    result->children.push_back(parseType(env));
    require(env, "Expected: ;", T::Symbol, ";");
    result->type = type;
    return result;
  };

  const auto parseExport = [&]() -> Ptr<Node> {
    env->i++;
    auto result = std::make_unique<Node>();
    result->children.push_back(parseExpr(env));
    require(env, "Expected: ;", T::Symbol, ";");
    result->type = N::ExportStatement;
    return result;
  };

  const auto parseImport = [&]() -> Ptr<Node> {
    env->i++;
    auto result = std::make_unique<Node>();
    result->children.push_back(parseExpr(env));
    require(env, "Expected: from", T::Identifier, "from");
    result->children.push_back(parsePath(env));
    require(env, "Expected: ;", T::Symbol, ";");
    result->type = N::ImportStatement;
    return result;
  };

  while (consume(env, T::Symbol, ";")) {}

  if (check(env, T::Keyword)) {
    const auto keyword = env->tokens[env->i].text;
    if (keyword == "const")     return parseDeclaration();
    if (keyword == "let")       return parseDeclaration();
    if (keyword == "if")        return parseIfStatement();
    if (keyword == "for")       return parseForLoop();
    if (keyword == "while")     return parseWhileLoop();
    if (keyword == "return")    return parseReturn();
    if (keyword == "break")     return parseControl();
    if (keyword == "continue")  return parseControl();
    if (keyword == "class")     return parseClassDeclaration(env);
    if (keyword == "type")      return parseTypeDecl(N::TypeAliasStatement);
    if (keyword == "interface") return parseTypeDecl(N::InterfaceStatement);
    if (keyword == "export")    return parseExport();
    if (keyword == "import")    return parseImport();
  }

  if (auto block = parseBlockStatement(env)) return block;
  return parseExprStatement(env);
}

Ptr<Node> parseProgram(Env* env) {
  auto result = std::make_unique<Node>();
  while (env->i < env->tokens.size()) {
    const size_t before = env->i;
    result->children.push_back(parseStatement(env));
    if (env->i == before) env->i++;
  }
  result->type = NodeType::Program;
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
    ss << tokenTypeName(x.type) << ": " << x.text << "\n";
  }
  return ss.str();
}

std::string formatAST(const ast::Node& node) {
  std::stringstream ss;

  const std::function<void(const ast::Node&, size_t)> recurse =
      [&](const auto& node, auto depth) {
    const std::string spacer = std::string(2 * depth, ' ');
    ss << spacer << ast::nodeTypeName(node.type);
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
