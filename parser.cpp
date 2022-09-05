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
    "break", "const", "continue", "for", "if", "let", "while",
  }};
  return result;
};

const Trie& symbolTrie() {
  static const Trie result{{
    "+", "+=", "-", "-=", "*", "*=", "/", "/=", "%", "**",
    "<", "<=", ">", ">=", "==", "!=", "===", "!==", "=",
    "(", ")", "[", "]", "{", "}", "=>", "?", ":", ".", ",", ";",
    "!", "~", "|", "&", "^", "&&", "||", "??", "<<", ">>",
  }};
  return result;
};

} // namespace trie

// Lexer routines.

namespace lexer {

enum class TokenType : uint8_t {
  Symbol,
  Keyword,
  Identifier,
  DblLiteral,
  IntLiteral,
  StrLiteral,
};

struct Token {
  TokenType type;
  std::string_view text;
};

struct LexerResult {
  std::vector<Token> tokens;
  std::vector<Diagnostic> diagnostics;
};

const char* tokenTypeName(TokenType type) {
  switch (type) {
    case TokenType::Symbol:     return "Symbol";
    case TokenType::Keyword:    return "Keyword";
    case TokenType::Identifier: return "Identifier";
    case TokenType::DblLiteral: return "DblLiteral";
    case TokenType::IntLiteral: return "IntLiteral";
    case TokenType::StrLiteral: return "StrLiteral";
  }
}

std::vector<Token> lex(
    const std::string& input, std::vector<Diagnostic>* diagnostics) {
  size_t i = 0;
  std::vector<Token> result;
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

  const auto make_token = [&](TokenType type, size_t pos, size_t end) -> Token {
    return {type, {input.data() + pos, end - pos}};
  };

  const auto parse_number = [&]() -> Token {
    const size_t pos = i;
    if (input[i] == '0') {
      if (input[i + 1] == 'x' && hexDigit(input[i + 2])) {
        for (i += 3; hexDigit(input[i]); i++) {}
        return make_token(TokenType::IntLiteral, pos, i);
      } else if (input[i + 1] == 'b' && binDigit(input[i + 2])) {
        for (i += 3; binDigit(input[i]); i++) {}
        return make_token(TokenType::IntLiteral, pos, i);
      } else if (digit(input[i + 1])) {
        i += 1;
        return make_token(TokenType::IntLiteral, pos, i);
      }
    }

    while (digit(input[i])) i++;

    const char ch = input[i];
    if (ch == '.') {
      for (i++; digit(input[i]); i++) {}
      return make_token(TokenType::DblLiteral, pos, i);
    } else if (ch == 'e' || ch == 'E') {
      if (input[i + 1] == '0') {
        i += 2;
        return make_token(TokenType::DblLiteral, pos, i);
      }
      for (i++; digit(input[i]); i++) {}
      return make_token(TokenType::DblLiteral, pos, i);
    }
    return make_token(TokenType::IntLiteral, pos, i);
  };

  const auto skip_whitespace = [&]{
    while (i < size) {
      const size_t pos = i;
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
        if (!ok) diagnostics->push_back({pos, "Unterminated /* comment"});
      } else {
        return;
      }
    }
  };

  while (i < size) {
    skip_whitespace();
    if (i == size) break;

    const size_t pos = i;
    const char ch = input[i];
    const size_t keywordMatched = trie::keywordTrie().match(input, i);
    if (keywordMatched && !identifier(input[i + keywordMatched])) {
      i += keywordMatched;
      result.push_back(make_token(TokenType::Keyword, pos, i));
    } else if (identifierStart(ch)) {
      for (i++; identifier(input[i]); i++) {}
      result.push_back(make_token(TokenType::Identifier, pos, i));
    } else if (digit(ch) || (ch == '.' && digit(input[i + 1]))) {
      result.push_back(parse_number());
      if (identifier(input[i])) {
        const char* error = (
          "A numeric literal cannot be immediately followed by an "
          "identifier, keyword, or numeric literal."
        );
        diagnostics->push_back({i, error});
        for (i++; identifier(input[i]); i++) {}
      }
    } else if (const size_t matched = consumeTrie(trie::symbolTrie())) {
      result.push_back(make_token(TokenType::Symbol, pos, i));
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

enum class NodeType : uint8_t {
  Program,
  // Statement nodes.
  ExprStatement,
  AssignmentStatement,
  // Expr nodes.
  BinOpExpr,
  TernaryExpr,
  AssignmentExpr,
  IdentifierExpr,
  DblLiteralExpr,
  IntLiteralExpr,
  StrLiteralExpr,
  // Miscellaneous nodes.
  Keyword,
  Operator,
  // Error placeholder.
  Error,
};

struct Node {
  Node() {}
  Node(const Node& o) = delete;
  Node& operator=(const Node& o) = delete;
  Node(Node&& o) = default;
  Node& operator=(Node&& o) = default;

  NodeType type = NodeType::Error;
  std::vector<Ptr<Node>> children;
  std::string_view source;
};

const char* nodeTypeName(NodeType type) {
  switch (type) {
    case NodeType::Program:             return "Program";
    case NodeType::ExprStatement:       return "ExprStatement";
    case NodeType::AssignmentStatement: return "AssignmentStatement";
    case NodeType::BinOpExpr:           return "BinOpExpr";
    case NodeType::TernaryExpr:         return "TernaryExpr";
    case NodeType::AssignmentExpr:      return "AssignmentExpr";
    case NodeType::IdentifierExpr:      return "IdentifierExpr";
    case NodeType::DblLiteralExpr:      return "DblLiteralExpr";
    case NodeType::IntLiteralExpr:      return "IntLiteralExpr";
    case NodeType::StrLiteralExpr:      return "StrLiteralExpr";
    case NodeType::Keyword:             return "Keyword";
    case NodeType::Operator:            return "Operator";
    case NodeType::Error:               return "Error";
  }
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

std::string_view source(Env* env, size_t pos, size_t end) {
  return {env->input.data() + pos, end - pos};
}

bool check(Env* env, TokenType type, const char* text = nullptr) {
  if (env->i == env->tokens.size()) return false;
  const auto& token = env->tokens[env->i];
  return token.type == type && (!text || token.text == text);
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

Ptr<Node> parseIdentifier(Env* env) {
  return parseToken(env, T::Identifier, N::IdentifierExpr, "Expected: identifier");
}

Ptr<Node> parseKeyword(Env* env) {
  return parseToken(env, T::Keyword, N::Keyword, "Expected: keyword");
}

Ptr<Node> parseOperator(Env* env) {
  return parseToken(env, T::Symbol, N::Operator, "Expected: operator");
}

Ptr<Node> parseExpr(Env* env);

Ptr<Node> parseTermExpr(Env* env) {
  const auto token = [&](NodeType type) {
    auto result = std::make_unique<Node>();
    result->source = env->tokens[env->i - 1].text;
    result->type = type;
    return result;
  };

  if (consume(env, T::Symbol, "(")) {
    auto result = parseExpr(env);
    require(env, "Expected: )", T::Symbol, ")");
    return result;
  }

  if (consume(env, T::Identifier)) return token(N::IdentifierExpr);
  if (consume(env, T::DblLiteral)) return token(N::DblLiteralExpr);
  if (consume(env, T::IntLiteral)) return token(N::IntLiteralExpr);
  if (consume(env, T::StrLiteral)) return token(N::StrLiteralExpr);

  env->diagnostics->push_back({cursor(env), "Expected: expression"});
  return std::make_unique<Node>();
}

Ptr<Node> parseBinOpExpr(Env* env) {
  using Op = std::pair<Ptr<Node>, ops::Precedence>;
  std::vector<Ptr<Node>> terms;
  std::vector<Op> ops;
  terms.push_back(parseTermExpr(env));
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
    terms.push_back(parseTermExpr(env));
  }

  while (!ops.empty()) evalOneOp();
  auto result = std::move(terms.back());
  terms.pop_back();
  return result;
}

Ptr<Node> parseExpr(Env* env) {
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

Ptr<Node> parseStatement(Env* env) {
  const auto pos = cursor(env);

  const auto parseAssignment = [&]() -> Ptr<Node> {
    auto result = std::make_unique<Node>();
    result->children.push_back(parseKeyword(env));
    result->children.push_back(parseIdentifier(env));
    require(env, "Expected: =", T::Symbol, "=");
    result->children.push_back(parseExpr(env));
    require(env, "Expected: ;", T::Symbol, ";");
    result->source = source(env, pos, cursor(env));
    result->type = N::AssignmentStatement;
    return result;
  };

  if (check(env, T::Keyword, "const") || check(env, T::Keyword, "let")) {
    return parseAssignment();
  }

  auto result = std::make_unique<Node>();
  result->children.push_back(parseExpr(env));
  require(env, "Expected: ;", T::Symbol, ";");
  result->source = source(env, pos, cursor(env));
  result->type = NodeType::ExprStatement;
  return result;
}

Ptr<Node> parseProgram(Env* env) {
  const auto pos = cursor(env);
  auto result = std::make_unique<Node>();
  while (env->i < env->tokens.size()) {
    const size_t before = env->i;
    result->children.push_back(parseStatement(env));
    if (env->i == before) env->i++;
  }
  result->source = source(env, pos, cursor(env));
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
  const auto program = parser::parse(input, tokens, &diagnostics);
  std::cerr << formatAST(*program);
  std::cerr << formatDiagnostics(input, &diagnostics);
  return diagnostics.empty() ? 0 : 1;
}
