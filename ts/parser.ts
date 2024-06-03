type int = number;

//////////////////////////////////////////////////////////////////////////////

// TokenSet

class TokenSet {
  private prefixes: Map<string, boolean>;

  constructor(items: string[]) {
    this.prefixes = new Map();
    for (const item of items) {
      for (let i = 1; i < item.length; i++) {
        this.prefixes.set(item.substring(0, i), false);
      }
    }
    for (const item of items) this.prefixes.set(item, true);
  }

  match(input: string, i: int): string | null {
    let result = null as string | null;
    for (let j = i + 1; j <= input.length; j++) {
      const prefix = input.substring(i, j);
      const lookup = this.prefixes.get(prefix);
      if (lookup === null) break;
      if (lookup === true) result = prefix;
    }
    return result;
  }
};

const keywords = new TokenSet([
  "break", "const", "continue", "class", "else", "export",
  "extends", "for", "if", "import", "private", "let", "new",
  "of", "return", "type", "while", "void", "true", "false",
]);

const ops = new TokenSet([
  "+", "+=", "-", "-=", "*", "*=", "/", "/=", "%", "**",
  "<", "<=", ">", ">=", "==", "!=", "===", "!==", "=",
  "(", ")", "[", "]", "{", "}", "=>", "?", ":", ".", ",", ";",
  "!", "~", "|", "&", "^", "&&", "||", "??", "<<", ">>", "++", "--",
]);

//////////////////////////////////////////////////////////////////////////////

// Lexing

enum TT {
  Symbol,
  Keyword,
  Identifier,
  DblLiteral,
  IntLiteral,
  StrLiteral,
  BoolLiteral,
  NullLiteral,
  VoidLiteral,
  TemplateStart,
  TemplateMid,
  TemplateEnd,
};

interface Diagnostic {
  pos: int,
  error: string,
};

interface Token {
  type: TT,
  text: string,
  pos: int,
  end: int,
};

const lex = (input: string, diagnostics: Diagnostic[]): Token[] => {
  let i = 0;
  const size = input.length;
  const result = [] as Token[];
  const braceStack = [] as boolean[];
  let symbol = null as string | null;

  const consume = (a: string): boolean => {
    const result = input[i] === a;
    if (result) i++;
    return result;
  };

  const consumeAll = (a: string): boolean => {
    for (let j = 0; j < a.length; j++) {
      if (input[i + j] !== a[j]) return false;
    }
    i += a.length;
    return true;
  };

  const consumeFromSet = (s: TokenSet): string | null => {
    const result = s.match(input, i);
    if (result === null) return result;
    i += result.length;
    return result;
  };

  const digit = (ch: string): boolean => '0' <= ch && ch <= '9';

  const binDigit = (ch: string): boolean => ch === '0' || ch === '1';

  const hexDigit = (ch: string): boolean => {
    return ('0' <= ch && ch <= '9') ||
           ('a' <= ch && ch <= 'f') ||
           ('A' <= ch && ch <= 'F');
  };

  const identifierStart = (ch: string): boolean => {
    return ('a' <= ch && ch <= 'z') ||
           ('A' <= ch && ch <= 'Z') ||
           ch === '_' || ch === '$';
  };

  const identifier = (ch: string): boolean => identifierStart(ch) || digit(ch);

  const makeToken = (type: TT, pos: int, end: int): Token => {
    return {type, text: input.substring(pos, end), pos, end};
  };

  const parseNumber = (): Token => {
    const pos = i;
    if (input[i] === '0') {
      const next = input[i + 1] ?? '';
      if (next === 'x' && i + 2 < size && hexDigit(input[i + 2]!)) {
        for (i += 3; i < size && hexDigit(input[i]!); i++) {}
        return makeToken(TT.IntLiteral, pos, i);
      } else if (next === 'b' && i + 2 < size && binDigit(input[i + 2]!)) {
        for (i += 3; i < size && binDigit(input[i]!); i++) {}
        return makeToken(TT.IntLiteral, pos, i);
      } else if (!digit(next)) {
        i += 1;
        return makeToken(TT.IntLiteral, pos, i);
      }
    }

    while (i < size && digit(input[i]!)) i++;

    const ch = input[i];
    if (ch === '.') {
      for (i++; i < size && digit(input[i]!); i++) {}
      return makeToken(TT.DblLiteral, pos, i);
    } else if (ch === 'e' || ch === 'E') {
      if (input[i + 1] === '0') {
        i += 2;
        return makeToken(TT.DblLiteral, pos, i);
      }
      for (i++; i < size && digit(input[i]!); i++) {}
      return makeToken(TT.DblLiteral, pos, i);
    }
    return makeToken(TT.IntLiteral, pos, i);
  };

  const parseHexDigits = (offset: int, n: int): boolean => {
    if (offset + n >= size) return false;
    for (let j = 0; j < n; j++) {
      if (!hexDigit(input[offset + j]!)) return false;
    }
    i += offset + n;
    return true;
  };

  const parseCharacter = (quote: string): boolean => {
    const ch = input[i] ?? '';
    if (ch === '\\') {
      const next = input[i + 1];
      if (next === '\\' || next === '0' || next === '"' || next === '\'' ||
          next === 'b' || next === 'n' || next === 'r' || next === 't') {
        return !!(i += 2);
      } else if ((next === 'x' && parseHexDigits(2, 2)) ||
                 (next === 'u' && parseHexDigits(2, 4))) {
        return true;
      }
      diagnostics.push({pos: i, error: "Unknown string escape sequence"});
      return !!(i += 1);
    } else if (ch !== '' && ch !== '\n' && ch !== '\r' && ch !== quote) {
      return !!(i += 1);
    }
    return false;
  };

  const parseString = (quote: string): Token => {
    const pos = i;
    i += 1;
    while (parseCharacter(quote)) {}
    if (input[i] !== quote) {
      diagnostics.push({pos: i, error: "Unterminated string literal"});
    } else {
      i++;
    }
    return makeToken(TT.StrLiteral, pos, i);
  };

  const parseTemplateCharacter = (): boolean => {
    const ch = input[i] ?? '';
    if (ch == '\\') {
      const next = input[i + 1];
      if (next == '`' || next == '$') return !!(i += 2);
      diagnostics.push({pos: i, error: "Unknown template escape sequence"});
      return !!(i += 1);
    } else if (ch != '' && ch != '`' && !(ch == '$' && input[i + 1] == '{')) {
      return !!(i += 1);
    }
    return false;
  };

  const parseTemplate = (quote: string): Token => {
    const pos = i;
    i += 1;
    while (parseTemplateCharacter()) {}
    if (input[i] == '`') {
      i += 1;
      const type = quote == '`' ? TT.StrLiteral : TT.TemplateEnd;
      return makeToken(type, pos, i);
    } else if (input[i] == '$' && input[i + 1] == '{') {
      i += 2;
      const type = quote == '`' ? TT.TemplateStart : TT.TemplateMid;
      return makeToken(type, pos, i);
    }
    diagnostics.push({pos: i, error: "Unterminated template literal"});
    const type = quote == '`' ? TT.TemplateStart : TT.TemplateMid;
    return makeToken(type, pos, i);
  };

  const skipWhitespace = (): void => {
    while (i < size) {
      const ch = input[i];
      if (ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t') {
        i++;
      } else if (consumeAll('//')) {
        for (; i < size; i++) {
          if (consume('\n')) break;
        }
      } else if (consumeAll('/*')) {
        let ok = false;
        for (i += 2; i < size && !ok; i++) {
          ok = consumeAll('*/');
        }
        if (!ok) diagnostics.push({pos: i, error: "Unterminated /* comment"});
      } else {
        return;
      }
    }
  };

  while (i < size) {
    skipWhitespace();
    if (i == size) break;

    const pos = i;
    const ch = input[i] ?? '';

    if (ch == '`') {
      const token = parseTemplate(ch);
      if (token.type == TT.TemplateStart) braceStack.push(true);
      result.push(token);
      continue;
    } else if (ch == '{') {
      result.push(makeToken(TT.Symbol, pos, ++i));
      braceStack.push(false);
      continue;
    } else if (ch == '}' && braceStack.length > 0) {
      if (braceStack[braceStack.length - 1]!) {
        const token = parseTemplate(ch);
        if (token.type == TT.TemplateEnd) braceStack.pop();
        result.push(token);
      } else {
        result.push(makeToken(TT.Symbol, pos, ++i));
        braceStack.pop();
      }
      continue;
    }

    const keyword = keywords.match(input, i);
    if (keyword && !identifier(input[i + keyword.length] ?? '')) {
      i += keyword.length;
      const token = makeToken(TT.Keyword, pos, i);
      result.push(token);
      const text = token.text;
      if (text === "true" || text === "false") {
        token.type = TT.BoolLiteral;
      } else if (text === "null") {
        token.type = TT.NullLiteral;
      } else if (text === "void") {
        token.type = TT.VoidLiteral;
      }
    } else if (ch == '"' || ch == '\'') {
      result.push(parseString(ch));
    } else if (identifierStart(ch)) {
      for (i++; i < size && identifier(input[i]!); i++) {}
      result.push(makeToken(TT.Identifier, pos, i));
    } else if (digit(ch) || (ch == '.' && i + 1 < size && digit(input[i + 1]!))) {
      result.push(parseNumber());
      if (i < size && identifier(input[i]!)) {
        const error = (
          "A numeric literal cannot be immediately followed by an " +
          "identifier, keyword, or numeric literal."
        );
        diagnostics.push({pos: i, error});
        for (i++; i < size && identifier(input[i]!); i++) {}
      }
    } else if (symbol = consumeFromSet(ops)) {
      result.push(makeToken(TT.Symbol, pos, i));
    } else {
      diagnostics.push({pos: i++, error: `Unknown symbol: ${ch}`});
    }
  }
  return result;
};

//////////////////////////////////////////////////////////////////////////////

// Parsing

interface Node<T> {
  kind: T,
  start: int,
  limit: int,
  children: Node<any>[],
};

enum NT {
  // Basic
  Identifier,
  Keyword,
  Operator,
  // Misc
  NameTypePair,
  // Types
  IdentifierType,
  ArrayType,
  TupleType,
  UnionType,
  StructType,
  ClosureType,
};

type TextNode<T> = Node<T> & {text: string};

type IdentifierNode = TextNode<NT.Identifier>;
type KeywordNode    = TextNode<NT.Keyword>;
type OperatorNode   = TextNode<NT.Operator>;

type NameTypePairNode =
    Node<NT.NameTypePair> & {name: IdentifierNode, type: TypeNode};

type IdentifierTypeNode =
    Node<NT.IdentifierType> & {text: string};
type ArrayTypeNode =
    Node<NT.ArrayType> & {element: TypeNode};
type TupleTypeNode =
    Node<NT.TupleType> & {elements: TypeNode[]};
type UnionTypeNode =
    Node<NT.TupleType> & {options: TypeNode[]};
type StructTypeNode =
    Node<NT.StructType> & {items: NameTypePairNode[]};
type ClosureTypeNode =
    Node<NT.ClosureType> & {args: NameTypePairNode[], result: TypeNode};

type TypeNode =
    IdentifierTypeNode |
    ArrayTypeNode |
    TupleTypeNode |
    UnionTypeNode |
    StructTypeNode |
    ClosureTypeNode;
