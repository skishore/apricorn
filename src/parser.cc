#include "parser.h"

#include <algorithm>
#include <array>
#include <cassert>
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

namespace {

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
      if (index >= node.children.size()) break;
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
    "of", "return", "type", "while", "true", "false",
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

using parser::Diagnostic;

#define TOKEN_TYPE \
  X(Symbol)        \
  X(Keyword)       \
  X(Identifier)    \
  X(DblLiteral)    \
  X(IntLiteral)    \
  X(StrLiteral)    \
  X(BoolLiteral)   \
  X(NullLiteral)   \
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
      const auto& text = result.back().text;
      if (text == "null") {
        result.back().type = TokenType::NullLiteral;
      } else if (text == "true" || text == "false") {
        result.back().type = TokenType::BoolLiteral;
      }
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

// Helper to look up symbols

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

namespace detail {

using namespace ast;
using namespace lexer;
using T = TokenType;

struct Env {
  const std::string& input;
  const std::vector<Token>& tokens;
  std::vector<Diagnostic>* diagnostics;
  size_t i;
};

bool advance(Env& env) {
  if (env.i == env.tokens.size()) return false;
  assert(env.i < env.tokens.size());
  env.i++;
  return true;
}

void assertAdvance(Env& env) {
  const bool okay = advance(env);
  assert(okay);
}

template <typename T>
T& append(Ptrs<Node>& children, Ptr<T> child) {
  T& result = *child;
  children.push_back(std::move(child));
  return result;
}

template <typename T>
T* append(Node& node, Ptr<T> child) {
  return &append(node.children, std::move(child));
}

size_t cursor(Env& env) {
  return env.i < env.tokens.size()
    ? static_cast<size_t>(env.tokens[env.i].text.data() - env.input.data())
    : env.input.size();
}

bool ahead(Env& env, size_t i, TokenType type, const char* text = nullptr) {
  if (env.i + i >= env.tokens.size()) return false;
  const auto& token = env.tokens[env.i + i];
  return token.type == type && (!text || token.text == text);
}

bool check(Env& env, TokenType type, const char* text = nullptr) {
  return ahead(env, 0, type, text);
}

bool consume(Env& env, TokenType type, const char* text = nullptr) {
  const auto result = check(env, type, text);
  if (result) assertAdvance(env);
  return result;
}

bool require(Env& env, const char* message,
             TokenType type, const char* text = nullptr) {
  if (consume(env, type, text)) return true;
  env.diagnostics->push_back({cursor(env), message});
  return false;
}

template <typename T, typename ...Args>
Ptr<T> node(Ptrs<Node>& children, Args&& ...args) {
    auto result = std::make_unique<T>(std::forward<Args>(args)...);
    result->children = std::move(children);
    return result;
}

template <typename T>
Ptr<T> parseToken(Env& env, TokenType tt, const char* error) {
  const auto source = require(env, error, tt)
      ? env.tokens[env.i - 1].text
      : std::string_view{};
  return std::make_unique<T>(source);
}

template <typename T>
Ptr<T> tokenNode(Env& env) {
    return std::make_unique<T>(env.tokens[env.i - 1].text);
}

Ptr<IdentifierNode> parseIdentifier(Env& env) {
  return parseToken<IdentifierNode>(env, T::Identifier, "Expected: identifier");
}

Ptr<KeywordNode> parseKeyword(Env& env) {
  return parseToken<KeywordNode>(env, T::Keyword, "Expected: keyword");
}

Ptr<OperatorNode> parseOperator(Env& env) {
  return parseToken<OperatorNode>(env, T::Symbol, "Expected: operator");
}

Ptr<IdentifierTypeNode> parseIdentifierType(Env& env) {
  return parseToken<IdentifierTypeNode>(env, T::Identifier, "Expected: type");
}

// Type grammar.

Ptr<TypeNode> parseType(Env& env);

Ptr<NameTypePairNode> parseNameTypePair(Env& env, const char* description) {
  auto children = Ptrs<Node>{};
  const auto& name = append(children, parseIdentifier(env));
  require(env, "Expected: :", T::Symbol, ":");
  const auto& type = append(children, parseType(env));
  return node<NameTypePairNode>(children, description, name, type);
}

Ptr<ClosureTypeNode> parseClosureType(Env& env) {
  const auto parseClosureBody =
      [&](Ptrs<Node> children, Refs<NameTypePairNode> args) {
    require(env, "Expected: =>", T::Symbol, "=>");
    const auto& result = append(children, parseType(env));
    return node<ClosureTypeNode>(children, std::move(args), result);
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

  if (check(env, T::Symbol, "(") && checkForArgList(1)) {
    require(env, "Expected: (", T::Symbol, "(");
    auto children = Ptrs<Node>{};
    auto args = Refs<NameTypePairNode>{};
    if (consume(env, T::Symbol, ")")) {
      return parseClosureBody(std::move(children), std::move(args));
    }
    do {
      args.push_back(append(children, parseNameTypePair(env, "Arg")));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")"));
    require(env, "Expected: )", T::Symbol, ")");
    return parseClosureBody(std::move(children), std::move(args));
  }

  return nullptr;
}

Ptr<TypeNode> parseQualifiedType(Env& env, bool lhs) {
  if (!check(env, T::Identifier) || !ahead(env, 1, T::Symbol, "<")) {
    return parseIdentifierType(env);
  }

  auto children = Ptrs<Node>{};
  auto generics = Refs<TypeNode>{};
  const auto& name = append(children, parseIdentifier(env));
  require(env, "Expected: <", TokenType::Symbol, "<");
  do {
    generics.push_back(append(children, parseType(env)));
  } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ">"));
  require(env, "Expected: >", T::Symbol, ">");
  return node<GenericTypeNode>(children, name, std::move(generics));
}

Ptr<TypeNode> parseRootType(Env& env) {
  if (auto closure = parseClosureType(env)) return closure;

  if (consume(env, T::Symbol, "(")) {
    auto result = parseType(env);
    require(env, "Expected: )", T::Symbol, ")");
    return result;
  }

  if (consume(env, T::Symbol, "[")) {
    auto children = Ptrs<Node>{};
    auto elements = Refs<TypeNode>{};
    if (consume(env, T::Symbol, "]")) {
      return node<TupleTypeNode>(children, std::move(elements));
    }
    do {
      elements.push_back(append(children, parseType(env)));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "]"));
    require(env, "Expected: ]", T::Symbol, "]");
    return node<TupleTypeNode>(children, std::move(elements));
  }

  if (consume(env, T::Symbol, "{")) {
    auto children = Ptrs<Node>{};
    auto items = Refs<NameTypePairNode>{};
    if (consume(env, T::Symbol, "}")) {
      return node<ObjectTypeNode>(children, std::move(items));
    }
    do {
      items.push_back(append(children, parseNameTypePair(env, "Item")));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "}"));
    require(env, "Expected: }", T::Symbol, "}");
    return node<ObjectTypeNode>(children, std::move(items));
  }

  return parseQualifiedType(env, false);
}

Ptr<TypeNode> parseTermType(Env& env) {
  auto result = parseRootType(env);
  if (consume(env, T::Symbol, "[")) {
    require(env, "Expected: ]", T::Symbol, "]");
    auto children = Ptrs<Node>{};
    const auto& element = append(children, std::move(result));
    return node<ArrayTypeNode>(children, element);
  }
  return result;
}

Ptr<TypeNode> parseType(Env& env) {
  auto result = parseTermType(env);
  if (!consume(env, T::Symbol, "|")) return result;

  auto children = Ptrs<Node>{};
  auto options = Refs<TypeNode>{};
  options.push_back(append(children, std::move(result)));
  do {
    options.push_back(append(children, parseTermType(env)));
  } while (consume(env, T::Symbol, "|"));
  return node<UnionTypeNode>(children, std::move(options));
}

// Expression grammar.

Ptr<ExprNode> parseExpr(Env& env);
Ptr<StatementNode> parseStatement(Env& env);
Ptr<BlockStatementNode> parseBlockStatement(Env& env);

Ptr<ArgDefinitionNode> parseArgDefinition(Env& env) {
  auto children = Ptrs<Node>{};
  const auto& name = append(children, parseIdentifier(env));
  auto* type = static_cast<const TypeNode*>(nullptr);
  if (consume(env, T::Symbol, ":")) {
    type = &append(children, parseType(env));
  }
  return node<ArgDefinitionNode>(children, name, type);
}

Ptr<CallArgsNode> parseCallArgs(Env& env) {
  auto children = Ptrs<Node>{};
  auto args = Refs<ExprNode>{};
  if (consume(env, T::Symbol, ")")) {
    return node<CallArgsNode>(children, std::move(args));
  }
  do {
    args.push_back(append(children, parseExpr(env)));
  } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")"));
  require(env, "Expected: )", T::Symbol, ")");
  return node<CallArgsNode>(children, std::move(args));
}

Ptr<ObjectItemNode> parseObjectItem(Env& env) {
  auto children = Ptrs<Node>{};
  const auto& name = append(children, parseIdentifier(env));
  auto& expr = [&]() -> ExprNode& {
    if (consume(env, T::Symbol, ":")) return append(children, parseExpr(env));
    auto expr = std::make_unique<IdentifierExprNode>(name.source);
    return append(children, std::move(expr));
  }();
  return node<ObjectItemNode>(children, name, expr);
}

Ptr<BlockStatementNode> parseFunctionBody(Env& env) {
    if (auto block = parseBlockStatement(env)) return block;

    auto statement = std::make_unique<ReturnStatementNode>();
    statement->expr = append(*statement, parseExpr(env));

    auto children = Ptrs<Node>{};
    auto statements = Refs<StatementNode>{};
    statements.push_back(append(children, std::move(statement)));
    return node<BlockStatementNode>(children, std::move(statements));
}

Ptr<ClosureExprNode> parseClosureExpr(Env& env) {
  const auto parseClosureBody =
      [&](Ptrs<Node> children, Refs<ArgDefinitionNode> args) {
    auto* type = static_cast<const TypeNode*>(nullptr);
    if (consume(env, T::Symbol, ":")) {
      type = &append(children, parseType(env));
    }
    require(env, "Expected: =>", T::Symbol, "=>");
    auto& body = append(children, parseFunctionBody(env));
    return node<ClosureExprNode>(children, std::move(args), type, body);
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
    auto children = Ptrs<Node>{};
    auto args = Refs<ArgDefinitionNode>{};
    args.push_back(append(children, parseArgDefinition(env)));
    return parseClosureBody(std::move(children), std::move(args));
  }

  if (ahead(env, 0, T::Symbol, "(") && checkForArgList(1)) {
    require(env, "Expected: (", T::Symbol, "(");
    auto children = Ptrs<Node>{};
    auto args = Refs<ArgDefinitionNode>{};
    if (consume(env, T::Symbol, ")")) {
      return parseClosureBody(std::move(children), std::move(args));
    }
    do {
      args.push_back(append(children, parseArgDefinition(env)));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")"));
    require(env, "Expected: )", T::Symbol, ")");
    return parseClosureBody(std::move(children), std::move(args));
  }
  return nullptr;
}

Ptr<ConstructorCallExprNode> parseConstructorCallExpr(Env& env) {
  if (!consume(env, T::Keyword, "new")) return nullptr;

  auto children = Ptrs<Node>{};
  const auto& cls  = append(children, parseIdentifier(env));
  require(env, "Expected: (", T::Symbol, "(");
  const auto& args = append(children, parseCallArgs(env));
  return node<ConstructorCallExprNode>(children, cls, args);
}

Ptr<ExprNode> parseRootExpr(Env& env) {
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
    auto children = Ptrs<Node>{};
    auto elements = Refs<ExprNode>{};
    if (consume(env, T::Symbol, "]")) {
      return node<ArrayExprNode>(children, std::move(elements));
    }
    do {
      elements.push_back(append(children, parseExpr(env)));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "]"));
    require(env, "Expected: ]", T::Symbol, "]");
    return node<ArrayExprNode>(children, std::move(elements));
  }

  if (consume(env, T::Symbol, "{")) {
    auto children = Ptrs<Node>{};
    auto items = Refs<ObjectItemNode>{};
    if (consume(env, T::Symbol, "}")) {
      return node<ObjectExprNode>(children, std::move(items));
    }
    do {
      items.push_back(append(children, parseObjectItem(env)));
    } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, "}"));
    require(env, "Expected: }", T::Symbol, "}");
    return node<ObjectExprNode>(children, std::move(items));
  }

  if (consume(env, T::TemplateStart)) {
    auto children = Ptrs<Node>{};
    auto suffixes = std::vector<TemplateExprNode::TemplatePair>{};
    const auto& prefix  = append(children, tokenNode<TemplateNode>(env));
    while (true) {
      const size_t before = env.i;
      auto& expr = append(children, parseExpr(env));
      if (consume(env, T::TemplateEnd)) {
        const auto& text = append(children, tokenNode<TemplateNode>(env));
        suffixes.push_back(TemplateExprNode::TemplatePair{expr, text});
        break;
      }
      require(env, "Expected: template", T::TemplateMid);
      const auto& text = append(children, tokenNode<TemplateNode>(env));
      suffixes.push_back(TemplateExprNode::TemplatePair{expr, text});
      if (env.i == before && !advance(env)) break;
    }
    return node<TemplateExprNode>(children, prefix, std::move(suffixes));
  }

  env.diagnostics->push_back({cursor(env), "Expected: expression"});
  return std::make_unique<ErrorExprNode>();
}

Ptr<ExprNode> parseTermExpr(Env& env) {
  auto expr = parseRootExpr(env);
  while (true) {
    if (!check(env, T::Symbol)) break;
    const auto symbol = env.tokens[env.i].text;

    if (symbol == "(" && ++env.i) {
      auto children = Ptrs<Node>{};
      auto& fn   = append(children, std::move(expr));
      auto& args = append(children, parseCallArgs(env));
      expr = node<FunctionCallExprNode>(children, fn, args);
      continue;
    }

    if (symbol == "." && ++env.i) {
      auto children = Ptrs<Node>{};
      auto& base  = append(children, std::move(expr));
      const auto& field = append(children, parseIdentifier(env));
      expr = node<FieldAccessExprNode>(children, base, field);
      continue;
    }

    if (symbol == "[" && ++env.i) {
      auto children = Ptrs<Node>{};
      auto& base  = append(children, std::move(expr));
      auto& index = append(children, parseExpr(env));
      require(env, "Expected: ]", T::Symbol, "]");
      expr = node<IndexAccessExprNode>(children, base, index);
      continue;
    }
    break;
  }
  return expr;
}

Ptr<ExprNode> parseUnaryOpExpr(Env& env) {
  Ptrs<OperatorNode> pre;
  const auto& preops  = ops::preops();
  const auto& postops = ops::postops();

  while (check(env, T::Symbol)) {
    const auto symbol = ops::key(env.tokens[env.i].text);
    if (preops.find(symbol) == preops.end()) break;
    pre.push_back(parseOperator(env));
  }

  auto expr = parseTermExpr(env);

  while (check(env, T::Symbol)) {
    const auto symbol = ops::key(env.tokens[env.i].text);
    if (postops.find(symbol) == postops.end()) break;
    auto children = Ptrs<Node>{};
    auto& e = append(children, std::move(expr));
    const auto& o = append(children, parseOperator(env));
    expr = node<UnarySuffixOpExprNode>(children, o, e);
  }

  while (!pre.empty()) {
    auto children = Ptrs<Node>{};
    const auto& o = append(children, std::move(pre.back()));
    auto& e = append(children, std::move(expr));
    expr = node<UnaryPrefixOpExprNode>(children, o, e);
    pre.pop_back();
  }
  return expr;
}

Ptr<ExprNode> parseBinaryOpExpr(Env& env) {
  using Op = std::pair<Ptr<OperatorNode>, ops::Precedence>;
  Ptrs<ExprNode> terms;
  std::vector<Op> ops;
  terms.push_back(parseUnaryOpExpr(env));
  const auto& binops = ops::binops();

  const auto evalOneOp = [&]{
    auto children = Ptrs<Node>{};
    auto& lhs = append(children, std::move(terms[terms.size() - 2]));
    const auto& op  = append(children, std::move(ops.back().first));
    auto& rhs = append(children, std::move(terms[terms.size() - 1]));
    auto result = node<BinaryOpExprNode>(children, op, lhs, rhs);

    ops.pop_back();
    terms.pop_back();
    terms.back() = std::move(result);
  };

  while (true) {
    if (!check(env, T::Symbol)) break;
    const auto pos = cursor(env);
    const auto symbol = ops::key(env.tokens[env.i].text);
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
          env.diagnostics->push_back({pos, ss.str()});
        }
      } else if (op.second.glb <= prev.second.lub) {
        std::stringstream ss;
        ss << "Ambiguous precedence: " << ops.back().first->source
           << " vs. " << op.first->source;
        env.diagnostics->push_back({pos, ss.str()});
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

Ptr<ExprNode> parseExpr(Env& env) {
  if (auto closure = parseClosureExpr(env)) return closure;
  if (auto ctor = parseConstructorCallExpr(env)) return ctor;

  auto base = parseBinaryOpExpr(env);
  if (!check(env, T::Symbol)) return base;

  if (consume(env, T::Symbol, "?")) {
    auto children = Ptrs<Node>{};
    auto& cond = append(children, std::move(base));
    auto& lhs  = append(children, parseExpr(env));
    require(env, "Expected: :", T::Symbol, ":");
    auto& rhs  = append(children, parseExpr(env));
    return node<TernaryExprNode>(children, cond, lhs, rhs);
  }

  const auto symbol = ops::key(env.tokens[env.i].text);
  const auto& assignment = ops::assignment();
  if (assignment.find(symbol) != assignment.end()) {
    auto children = Ptrs<Node>{};
    auto& lhs = append(children, std::move(base));
    const auto& op  = append(children, parseOperator(env));
    auto& rhs = append(children, parseExpr(env));
    return node<AssignmentExprNode>(children, op, lhs, rhs);
  }

  return base;
}

// Statement grammar.

Ptr<StatementNode> parseTrivialStatement(
    Env& env, const char* message, const char* text) {
  if (consume(env, T::Symbol, text)) {
    return std::make_unique<EmptyStatementNode>();
  }
  auto children = Ptrs<Node>{};
  auto& expr = append(children, parseExpr(env));
  require(env, message, T::Symbol, text);
  return node<ExprStatementNode>(children, expr);
}

Ptr<StatementNode> parseTrivialStatementAndSemiColon(Env& env) {
  return parseTrivialStatement(env, "Expected: ;", ";");
}

Ptr<StatementNode> parseTrivialStatementAndParen(Env& env) {
  return parseTrivialStatement(env, "Expected: )", ")");
}

Ptr<BlockStatementNode> parseBlockStatement(Env& env) {
  if (!consume(env, T::Symbol, "{")) return nullptr;
  auto children = Ptrs<Node>{};
  auto statements = Refs<StatementNode>{};
  while (!consume(env, T::Symbol, "}")) {
    const size_t before = env.i;
    statements.push_back(append(children, parseStatement(env)));
    if (env.i == before && !advance(env)) break;
  }
  return node<BlockStatementNode>(children, std::move(statements));
}

void parseClassComponent(Env& env, Ptrs<Node>& siblings,
                         Refs<ClassMemberNode>& members,
                         Refs<ClassMethodNode>& methods) {
  auto children = Ptrs<Node>{};
  auto* access = static_cast<const KeywordNode*>(nullptr);
  if (check(env, T::Keyword, "private")) access = &append(children, parseKeyword(env));
  const auto& name = append(children, parseIdentifier(env));

  if (consume(env, T::Symbol, "(")) {
    auto args = Refs<ArgDefinitionNode>{};
    if (!consume(env, T::Symbol, ")")) {
      do {
        args.push_back(append(children, parseArgDefinition(env)));
      } while (consume(env, T::Symbol, ",") && !check(env, T::Symbol, ")"));
      require(env, "Expected: )", T::Symbol, ")");
    }
    auto* type = static_cast<const TypeNode*>(nullptr);
    if (consume(env, T::Symbol, ":")) type = &append(children, parseType(env));
    auto& body = append(children, parseFunctionBody(env));
    auto result = node<ClassMethodNode>(
          children, access, name, std::move(args), type, body);
    methods.push_back(append(siblings, std::move(result)));
    return;
  }

  auto* type = static_cast<const TypeNode*>(nullptr);
  if (consume(env, T::Symbol, ":")) {
    type = &append(children, parseType(env));
  }
  auto* init = static_cast<ExprNode*>(nullptr);
  if (consume(env, T::Symbol, "=")) {
    init = &append(children, parseExpr(env));
  }
  require(env, "Expected: ;", T::Symbol, ";");
  auto result = node<ClassMemberNode>(children, access, name, type, init);
  members.push_back(append(siblings, std::move(result)));
}

Ptr<ClassDeclarationStatementNode> parseClassDeclaration(Env& env) {
  auto children = Ptrs<Node>{};
  require(env, "Expected: class", T::Keyword, "class");
  const auto& name = append(children, parseIdentifier(env));
  auto base = static_cast<const IdentifierNode*>(nullptr);
  if (consume(env, T::Keyword, "extends")) {
    base = &append(children, parseIdentifier(env));
  }
  require(env, "Expected: {", T::Symbol, "{");
  auto members = Refs<ClassMemberNode>{};
  auto methods = Refs<ClassMethodNode>{};
  while (!consume(env, T::Symbol, "}")) {
    const size_t before = env.i;
    parseClassComponent(env, children, members, methods);
    if (env.i == before && !advance(env)) break;
  }
  require(env, "Expected: ;", T::Symbol, ";");
  return node<ClassDeclarationStatementNode>(
      children, name, base, std::move(members), std::move(methods));
}

Ptr<StatementNode> parseStatement(Env& env) {
  const auto parseDeclaration = [&]() -> Ptr<StatementNode> {
    auto children = Ptrs<Node>{};
    const auto& keyword = append(children, parseKeyword(env));
    auto& root = append(children, parseRootExpr(env));
    auto* type = static_cast<const TypeNode*>(nullptr);
    if (consume(env, T::Symbol, ":")) {
      type = &append(children, parseType(env));
    }
    require(env, "Expected: =", T::Symbol, "=");
    auto& expr = append(children, parseExpr(env));
    require(env, "Expected: ;", T::Symbol, ";");
    return node<DeclarationStatementNode>(children, keyword, root, type, expr);
  };

  const auto parseCond = [&]() -> Ptr<CondClauseNode> {
    auto children = Ptrs<Node>{};
    require(env, "Expected: (", T::Symbol, "(");
    auto& cond = append(children, parseExpr(env));
    require(env, "Expected: )", T::Symbol, ")");
    auto& then = append(children, parseStatement(env));
    return node<CondClauseNode>(children, cond, then);
  };

  const auto parseIfStatement = [&]() -> Ptr<StatementNode> {
    assertAdvance(env);
    auto children = Ptrs<Node>{};
    auto cases = Refs<CondClauseNode>{};
    auto* elseCase = static_cast<StatementNode*>(nullptr);
    cases.push_back(append(children, parseCond()));
    while (elseCase == nullptr && consume(env, T::Keyword, "else")) {
      if (consume(env, T::Keyword, "if")) {
        cases.push_back(append(children, parseCond()));
      } else {
        elseCase = &append(children, parseStatement(env));
      }
    }
    return node<IfStatementNode>(children, std::move(cases), elseCase);
  };

  const auto parseForLoop = [&]() -> Ptr<StatementNode> {
    assertAdvance(env);
    auto result = std::unique_ptr<ForLoopStatementNode>();
    require(env, "Expected: (", T::Symbol, "(");

    if (check(env, T::Keyword, "const") || check(env, T::Keyword, "let")) {
      auto keywordNode = parseKeyword(env);
      auto rootNode = parseRootExpr(env);
      auto typeNode = std::unique_ptr<TypeNode>();

      if (consume(env, T::Symbol, ":")) typeNode = parseType(env);

      if (consume(env, T::Keyword, "of")) {
        auto result = std::make_unique<ForEachStatementNode>();
        result->init.keyword = append(*result, std::move(keywordNode));
        result->init.root = append(*result, std::move(rootNode));
        if (typeNode) result->init.type = append(*result, std::move(typeNode));
        result->init.expr = append(*result, parseExpr(env));
        require(env, "Expected: )", T::Symbol, ")");
        result->body = append(*result, parseStatement(env));
        return result;
      }

      auto init = [&]{
        auto children = Ptrs<Node>{};
        require(env, "Expected: =", T::Symbol, "=");
        const auto& keyword = append(children, std::move(keywordNode));
        auto& root = append(children, std::move(rootNode));
        auto type = static_cast<const TypeNode*>(nullptr);
        if (typeNode) type = &append(children, std::move(typeNode));
        auto& expr = append(children, parseExpr(env));
        return node<DeclarationStatementNode>(children, keyword, root, type, expr);
      }();
      require(env, "Expected: ;", T::Symbol, ";");
      result = std::make_unique<ForLoopStatementNode>();
      result->init = append(*result, std::move(init));
    } else {
      result = std::make_unique<ForLoopStatementNode>();
      result->init = append(*result, parseTrivialStatementAndSemiColon(env));
    }

    result->cond = append(*result, parseExpr(env));
    require(env, "Expected: ;", T::Symbol, ";");
    result->post = append(*result, parseTrivialStatementAndParen(env));
    result->body = append(*result, parseStatement(env));
    return result;
  };

  const auto parseWhileLoop = [&]() -> Ptr<StatementNode> {
    assertAdvance(env);
    auto result = std::make_unique<WhileLoopStatementNode>();
    require(env, "Expected: (", T::Symbol, "(");
    result->cond = append(*result, parseExpr(env));
    require(env, "Expected: )", T::Symbol, ")");
    result->body = append(*result, parseStatement(env));
    return result;
  };

  const auto parseReturn = [&]() -> Ptr<StatementNode> {
    assertAdvance(env);
    auto result = std::make_unique<ReturnStatementNode>();
    if (!consume(env, T::Symbol, ";")) {
      result->expr = append(*result, parseExpr(env));
      require(env, "Expected: ;", T::Symbol, ";");
    }
    return result;
  };

  const auto parseTypeAlias = [&]() -> Ptr<StatementNode> {
    assertAdvance(env);
    auto result = std::make_unique<TypeAliasStatementNode>();
    result->lhs = append(*result, parseIdentifier(env));
    require(env, "Expected: =", T::Symbol, "=");
    result->rhs = append(*result, parseType(env));
    require(env, "Expected: ;", T::Symbol, ";");
    return result;
  };

  if (check(env, T::Keyword)) {
    const auto keyword = env.tokens[env.i].text;
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
  return parseTrivialStatementAndSemiColon(env);
}

Ptr<ProgramNode> parseProgram(Env& env) {
  auto result = std::make_unique<ProgramNode>();
  while (env.i < env.tokens.size()) {
    const size_t before = env.i;
    result->statements.push_back(*append(*result, parseStatement(env)));
    if (env.i == before) assertAdvance(env);
  }
  return result;
}

} // namespace detail

} // namespace

// Parsing entry points

namespace parser {

Ptr<ProgramNode> parse(const std::string& input,
                       std::vector<Diagnostic>* diagnostics) {
  const auto tokens = lexer::lex(input, diagnostics);
  detail::Env env{input, tokens, diagnostics, 0};
  return detail::parseProgram(env);
}

[[maybe_unused]] std::string formatTokens(const std::vector<lexer::Token>& tokens) {
  std::stringstream ss;
  for (const auto& x : tokens) {
    ss << desc(x.type) << ": " << x.text << "\n";
  }
  return ss.str();
}

std::string formatAST(const Node& node) {
  std::stringstream ss;

  const std::function<void(const Node&, size_t)> recurse =
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

} // namespace parser
