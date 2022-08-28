#include <array>
#include <cstdint>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

// Quick-and-dirty arena-allocated trie.

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
    for (size_t end = pos; end < input.size(); end++) {
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
    "<", "<=", ">", ">=", "===", "!==", "=", ".", ",", ";",
    "(", ")", "[", "]", "{", "}", "=>",
    "!", "~", "|", "&", "^", "&&", "||",
  }};
  return result;
};

// Lexer routines.

enum class TokenType : uint8_t {
  Symbol,
  Keyword,
  Identifier,
  DblLiteral,
  IntLiteral,
  StrLiteral,
};

struct Diagnostic {
  size_t pos;
  std::string error;
};

struct Token {
  size_t pos;
  size_t end;
  TokenType type;
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

LexerResult lex(const std::string& input) {
  size_t i = 0;
  LexerResult result;

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

  const auto consumeTrie = [&](const Trie& trie) {
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

  const auto parse_number = [&]() -> Token {
    const size_t pos = i;
    if (input[i] == '0') {
      if (input[i + 1] == 'x' && hexDigit(input[i + 2])) {
        for (i += 3; hexDigit(input[i]); i++) {}
        return {pos, i, TokenType::IntLiteral};
      } else if (input[i + 1] == 'b' && binDigit(input[i + 2])) {
        for (i += 3; binDigit(input[i]); i++) {}
        return {pos, i, TokenType::IntLiteral};
      } else if (digit(input[i + 1])) {
        i += 1;
        return {pos, i, TokenType::IntLiteral};
      }
    }

    while (digit(input[i])) i++;

    const char ch = input[i];
    if (ch == '.') {
      for (i++; digit(input[i]); i++) {}
      return {pos, i, TokenType::DblLiteral};
    } else if (ch == 'e' || ch == 'E') {
      if (input[i + 1] == '0') {
        i += 2;
        return {pos, i, TokenType::DblLiteral};
      }
      for (i++; digit(input[i]); i++) {}
      return {pos, i, TokenType::DblLiteral};
    }
    return {pos, i, TokenType::IntLiteral};
  };

  const auto skip_whitespace = [&]{
    while (i < input.size()) {
      const size_t pos = i;
      const char ch = input[i];
      if (ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t') {
        i++;
      } else if (consumeAll(2, "//")) {
        for (; i < input.size(); i++) {
          if (consume('\n')) break;
        }
      } else if (consumeAll(2, "/*")) {
        auto ok = false;
        for (i += 2; i < input.size() && !ok; i++) {
          ok = consumeAll(2, "*/");
        }
        if (!ok) result.diagnostics.push_back({pos, "Unterminated /* comment"});
      } else {
        return;
      }
    }
  };

  while (i < input.size()) {
    skip_whitespace();
    if (i == input.size()) break;

    const size_t pos = i;
    const char ch = input[i];
    const size_t keywordMatched = keywordTrie().match(input, i);
    if (keywordMatched && !identifier(input[i + keywordMatched])) {
      i += keywordMatched;
      result.tokens.push_back({pos, i, TokenType::Keyword});
    } else if (identifierStart(ch)) {
      for (i++; identifier(input[i]); i++) {}
      result.tokens.push_back({pos, i, TokenType::Identifier});
    } else if (digit(ch) || (ch == '.' && digit(input[i + 1]))) {
      result.tokens.push_back(parse_number());
      if (identifier(input[i])) {
        const char* error = (
          "A numeric literal cannot be immediately followed by an "
          "identifier, keyword, or numeric literal."
        );
        result.diagnostics.push_back({i, error});
        for (i++; identifier(input[i]); i++) {}
      }
    } else if (const size_t matched = consumeTrie(symbolTrie())) {
      result.tokens.push_back({pos, i, TokenType::Symbol});
    } else {
      const auto error = std::string("Unknown symbol: ") + ch;
      result.diagnostics.push_back({i++, error});
    }
  }
  return result;
}

int main(int argc, const char** argv) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " $FILE" << std::endl;
    return 1;
  }

  std::ifstream input(argv[1]);
  std::stringstream ss;
  while (input >> ss.rdbuf());
  const auto data = ss.str();

  const auto result = lex(data);
  for (const auto& x : result.tokens) {
    std::cerr << tokenTypeName(x.type) << ": "
              << data.substr(x.pos, x.end - x.pos) << std::endl;
  }
  for (const auto& x : result.diagnostics) {
    std::cerr << "Error at " << x.pos << ": " << x.error << std::endl;
  }
  return result.diagnostics.empty() ? 0 : 1;
}
