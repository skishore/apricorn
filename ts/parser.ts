type int = number;

const assert = (x: boolean) => { if (!x) throw Error(); }

//////////////////////////////////////////////////////////////////////////////

// TokenSet

interface TokenSet {
  prefixes: Map<string, boolean>;
};

const makeTokenSet = (items: string[]): TokenSet => {
  const prefixes = new Map();
  for (const item of items) {
    for (let i = 1; i < item.length; i++) {
      prefixes.set(item.substring(0, i), false);
    }
  }
  for (const item of items) prefixes.set(item, true);
  return {prefixes};
};

const matchTokenSet = (set: TokenSet, input: string, i: int): string | null => {
  let result = null as string | null;
  for (let j = i + 1; j <= input.length; j++) {
    const prefix = input.substring(i, j);
    const lookup = set.prefixes.get(prefix) ?? null;
    if (lookup === null) break;
    if (lookup === true) result = prefix;
  }
  return result;
};

const keywords = makeTokenSet([
  'break', 'const', 'continue', 'class', 'else', 'export',
  'extends', 'for', 'if', 'import', 'private', 'let', 'new',
  'of', 'return', 'type', 'while', 'void', 'true', 'false',
]);

const ops = makeTokenSet([
  '+', '+=', '-', '-=', '*', '*=', '/', '/=', '%', '**',
  '<', '<=', '>', '>=', '==', '!=', '===', '!==', '=',
  '(', ')', '[', ']', '{', '}', '=>', '?', ':', '.', ',', ';',
  '!', '~', '|', '&', '^', '&&', '||', '??', '<<', '>>', '++', '--',
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

  const consumeFromSet = (set: TokenSet): string | null => {
    const result = matchTokenSet(set, input, i);
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
    assert(pos < end);
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
      diagnostics.push({pos: i, error: 'Unknown string escape sequence'});
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
      diagnostics.push({pos: i, error: 'Unterminated string literal'});
    } else {
      i++;
    }
    return makeToken(TT.StrLiteral, pos, i);
  };

  const parseTemplateCharacter = (): boolean => {
    const ch = input[i] ?? '';
    if (ch === '\\') {
      const next = input[i + 1];
      if (next === '`' || next === '$') return !!(i += 2);
      diagnostics.push({pos: i, error: 'Unknown template escape sequence'});
      return !!(i += 1);
    } else if (ch !== '' && ch !== '`' && !(ch === '$' && input[i + 1] === '{')) {
      return !!(i += 1);
    }
    return false;
  };

  const parseTemplate = (quote: string): Token => {
    const pos = i;
    i += 1;
    while (parseTemplateCharacter()) {}
    if (input[i] === '`') {
      i += 1;
      const type = quote === '`' ? TT.StrLiteral : TT.TemplateEnd;
      return makeToken(type, pos, i);
    } else if (input[i] === '$' && input[i + 1] === '{') {
      i += 2;
      const type = quote === '`' ? TT.TemplateStart : TT.TemplateMid;
      return makeToken(type, pos, i);
    }
    diagnostics.push({pos: i, error: 'Unterminated template literal'});
    const type = quote === '`' ? TT.TemplateStart : TT.TemplateMid;
    return makeToken(type, pos, i);
  };

  const skipWhitespace = (): void => {
    const start = '/' + '*';
    const limit = '*' + '/';
    while (i < size) {
      const ch = input[i];
      if (ch === ' ' || ch === '\n' || ch === '\r' || ch === '\t') {
        i++;
      } else if (consumeAll('//')) {
        for (; i < size; i++) if (consume('\n')) break;
      } else if (consumeAll(start)) {
        let ok = false;
        for (; i < size && !ok; i++) ok = consumeAll(limit);
        if (!ok) diagnostics.push({pos: i, error: `Unterminated ${start} comment`});
      } else {
        return;
      }
    }
  };

  while (i < size) {
    skipWhitespace();
    assert(i <= size);
    if (i === size) break;

    const pos = i;
    const ch = input[i] ?? '';

    if (ch === '`') {
      const token = parseTemplate(ch);
      if (token.type === TT.TemplateStart) braceStack.push(true);
      result.push(token);
      continue;
    } else if (ch === '{') {
      result.push(makeToken(TT.Symbol, pos, ++i));
      braceStack.push(false);
      continue;
    } else if (ch === '}' && braceStack.length > 0) {
      if (braceStack[braceStack.length - 1]!) {
        const token = parseTemplate(ch);
        if (token.type === TT.TemplateEnd) braceStack.pop();
        result.push(token);
      } else {
        result.push(makeToken(TT.Symbol, pos, ++i));
        braceStack.pop();
      }
      continue;
    }

    const keyword = matchTokenSet(keywords, input, i);
    if (keyword && !identifier(input[i + keyword.length] ?? '')) {
      i += keyword.length;
      const token = makeToken(TT.Keyword, pos, i);
      result.push(token);
      const text = token.text;
      if (text === 'true' || text === 'false') {
        token.type = TT.BoolLiteral;
      } else if (text === 'null') {
        token.type = TT.NullLiteral;
      } else if (text === 'void') {
        token.type = TT.VoidLiteral;
      }
    } else if (ch === '"' || ch === '\'') {
      result.push(parseString(ch));
    } else if (identifierStart(ch)) {
      for (i++; i < size && identifier(input[i]!); i++) {}
      result.push(makeToken(TT.Identifier, pos, i));
    } else if (digit(ch) || (ch === '.' && i + 1 < size && digit(input[i + 1]!))) {
      result.push(parseNumber());
      if (i < size && identifier(input[i]!)) {
        const error = (
          'A numeric literal cannot be immediately followed by an ' +
          'identifier, keyword, or numeric literal.'
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

// AST

enum NT {
  // Basic
  Identifier,
  Keyword,
  Operator,
  Template,
  // Misc
  NameTypePair,
  // Types
  ArrayType,
  ErrorType,
  TupleType,
  UnionType,
  StructType,
  ClosureType,
  GenericType,
  IdentifierType,
};

interface BaseNode {
  pos: int,
  end: int,
  text: string,
  children: Node[],
};

type IdentifierNode = {base: BaseNode, kind: NT.Identifier};
type KeywordNode    = {base: BaseNode, kind: NT.Keyword};
type OperatorNode   = {base: BaseNode, kind: NT.Operator};
type TemplateNode   = {base: BaseNode, kind: NT.Template};

type NameTypePairNode =
  {base: BaseNode, kind: NT.NameTypePair, name: IdentifierNode, type: TypeNode};

type ArrayTypeNode =
  {base: BaseNode, kind: NT.ArrayType, element: TypeNode};
type ErrorTypeNode =
  {base: BaseNode, kind: NT.ErrorType};
type TupleTypeNode =
  {base: BaseNode, kind: NT.TupleType, elements: TypeNode[]};
type UnionTypeNode =
  {base: BaseNode, kind: NT.UnionType, options: TypeNode[]};
type StructTypeNode =
  {base: BaseNode, kind: NT.StructType, items: NameTypePairNode[]};
type ClosureTypeNode =
  {base: BaseNode, kind: NT.ClosureType, args: NameTypePairNode[], result: TypeNode};
type GenericTypeNode =
  {base: BaseNode, kind: NT.GenericType, name: IdentifierNode, generics: TypeNode[]};
type IdentifierTypeNode =
  {base: BaseNode, kind: NT.IdentifierType};

type TypeNode =
    ArrayTypeNode |
    ErrorTypeNode |
    TupleTypeNode |
    UnionTypeNode |
    StructTypeNode |
    ClosureTypeNode |
    GenericTypeNode |
    IdentifierTypeNode;

type Node =
    IdentifierNode |
    KeywordNode |
    OperatorNode |
    TemplateNode |
    NameTypePairNode |
    TypeNode;

//////////////////////////////////////////////////////////////////////////////

// Parsing

interface Env {
  input: string,
  tokens: Token[],
  diagnostics: Diagnostic[],
  i: int,
};

const advance = (env: Env): boolean => {
  if (env.i === env.tokens.length) return false;
  assert(env.i < env.tokens.length);
  env.i++;
  return true;
};

const assertAdvance = (env: Env): void => {
  const okay = advance(env);
  assert(okay);
};

const append = (node: BaseNode, child: Node): void => {
  node.pos = Math.min(node.pos, child.base.pos);
  node.end = Math.max(node.end, child.base.end);
  node.children.push(child);
};

const cursor = (env: Env): int => {
  const i = env.i;
  const tokens = env.tokens;
  if (tokens.length === 0) return 0;
  return i < tokens.length ? tokens[i]!.pos : tokens[tokens.length - 1]!.end;
};

const ahead = (env: Env, i: int, type: TT, text: string | null = null): boolean => {
  if (env.i + i >= env.tokens.length) return false;
  const token = env.tokens[env.i + i]!;
  return token.type === type && (text === null || token.text === text);
};

const check = (env: Env, type: TT, text: string | null = null): boolean => {
  return ahead(env, 0, type, text);
};

const consume = (env: Env, node: BaseNode | null,
                 type: TT, text: string | null = null): boolean => {
  if (!check(env, type, text)) return false;
  const token = env.tokens[env.i++]!;
  if (node) includeToken(node, token);
  return true;
};

const expect = (env: Env, message: string, node: BaseNode | null,
                type: TT, text: string | null = null): boolean => {
  if (consume(env, node, type, text)) return true;
  env.diagnostics.push({pos: cursor(env), error: message});
  return false;
}

const includeToken = (node: BaseNode, token: Token) => {
  node.pos = Math.min(node.pos, token.pos);
  node.end = Math.max(node.pos, token.end);
};

const makeBaseNode = (env: Env): BaseNode => {
  const {i, tokens} = env;
  const result = {pos: 0, end: 0, text: '', children: []};
  if (i < tokens.length) {
    const token = tokens[i]!;
    result.pos = result.end = token.pos;
  } else if (tokens.length > 0) {
    const token = tokens[tokens.length - 1]!;
    result.pos = result.end = token.end;
  }
  return result;
};

// Basic nodes

const curTokenText = (env: Env, message: string, base: BaseNode,
                      type: TT, alt: string): string => {
  return expect(env, message, base, type) ? env.tokens[env.i - 1]!.text : alt;
};

const parseIdentifier = (env: Env, alt: string): IdentifierNode => {
  const base = makeBaseNode(env);
  base.text = curTokenText(env, 'Expected: identifier', base, TT.Identifier, alt);
  return {base, kind: NT.Identifier};
};

const parseKeyword = (env: Env): KeywordNode => {
  const base = makeBaseNode(env);
  base.text = curTokenText(env, 'Expected: keyword', base, TT.Keyword, '');
  return {base, kind: NT.Keyword};
};

const parseOperator = (env: Env): OperatorNode => {
  const base = makeBaseNode(env);
  base.text = curTokenText(env, 'Expected: operator', base, TT.Symbol, '');
  return {base, kind: NT.Operator};
};

const parseIdentifierType = (env: Env, alt: string): IdentifierTypeNode => {
  const base = makeBaseNode(env);
  base.text = curTokenText(env, 'Expected: type', base, TT.Identifier, alt);
  return {base, kind: NT.IdentifierType};
};

// Type grammar

const parseNameTypePair = (env: Env, alt: string): NameTypePairNode => {
  const base = makeBaseNode(env);
  const name = parseIdentifier(env, alt);
  append(base, name);
  expect(env, 'Expected: :', base, TT.Symbol, ':');
  const type = parseType(env);
  append(base, type);
  return {base, kind: NT.NameTypePair, name, type};
};

const parseClosureType = (env: Env): ClosureTypeNode | null => {
  const parseBody = (base: BaseNode, args: NameTypePairNode[]): ClosureTypeNode => {
    expect(env, 'Expected: =>', base, TT.Symbol, '=>');
    const result = parseType(env);
    append(base, result);
    return {base, kind: NT.ClosureType, args, result};
  };

  const checkForArrow = (i: int): boolean => {
    return ahead(env, i, TT.Symbol, ')') && ahead(env, i + 1, TT.Symbol, '=>');
  };

  const checkForArgList = (i: int): boolean => {
    if (checkForArrow(i)) return true;
    if (!ahead(env, i, TT.Identifier)) return false;
    return ahead(env, i + 1, TT.Symbol, ',') ||
           ahead(env, i + 1, TT.Symbol, ':') ||
           checkForArrow(i + 1);
  };

  if (check(env, TT.Symbol, '(') && checkForArgList(1)) {
    const base = makeBaseNode(env);
    expect(env, 'Expected: (', base, TT.Symbol, '(');
    const args = [] as NameTypePairNode[];
    if (consume(env, base, TT.Symbol, ')')) return parseBody(base, args);
    do {
      args.push(parseNameTypePair(env, `$${args.length}`));
      append(base, args[args.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, ')'));
    expect(env, 'Expected: )', base, TT.Symbol, ')');
    return parseBody(base, args);
  }
  return null;
};

const parseQualifiedType = (env: Env): TypeNode => {
  if (check(env, TT.NullLiteral) || check(env, TT.VoidLiteral)) {
    const base = makeBaseNode(env);
    const token = env.tokens[env.i++]!;
    includeToken(base, token);
    base.text = token.text;
    return {base, kind: NT.IdentifierType};
  }

  if (!check(env, TT.Identifier)) {
    env.diagnostics.push({pos: cursor(env), error: 'Expected: type'});
    return {base: makeBaseNode(env), kind: NT.ErrorType};
  } else if (!ahead(env, 1, TT.Symbol, '<')) {
    return parseIdentifierType(env, '');
  }

  const base = makeBaseNode(env);
  const name = parseIdentifier(env, '');
  append(base, name);
  const generics = [] as TypeNode[];
  expect(env, 'Expected: <', base, TT.Symbol, '<');
  do {
    generics.push(parseType(env));
    append(base, generics[generics.length - 1]!);
  } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, '>'));
  expect(env, 'Expected: >', base, TT.Symbol, '>');
  return {base, kind: NT.GenericType, name, generics};
};

const parseRootType = (env: Env): TypeNode => {
  const closure = parseClosureType(env);
  if (closure) return closure;

  if (consume(env, null, TT.Symbol, '(')) {
    const result = parseType(env);
    expect(env, 'Expected: )', null, TT.Symbol, ')');
    return result;
  }

  if (check(env, TT.Symbol, '[')) {
    const base = makeBaseNode(env);
    const elements = [] as TypeNode[];
    expect(env, 'Expected: [', base, TT.Symbol, '[');
    if (consume(env, base, TT.Symbol, ']')) {
      return {base, kind: NT.TupleType, elements};
    }
    do {
      elements.push(parseType(env));
      append(base, elements[elements.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, ']'));
    expect(env, 'Expected: ]', base, TT.Symbol, ']');
    return {base, kind: NT.TupleType, elements};
  }

  if (check(env, TT.Symbol, '{')) {
    const base = makeBaseNode(env);
    const items = [] as NameTypePairNode[];
    expect(env, 'Expected: [', base, TT.Symbol, '[');
    if (consume(env, base, TT.Symbol, '}')) {
      return {base, kind: NT.StructType, items};
    }
    do {
      items.push(parseNameTypePair(env, 'Item'));
      append(base, items[items.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, '}'));
    expect(env, 'Expected: }', base, TT.Symbol, '}');
    return {base, kind: NT.StructType, items};
  }
  return parseQualifiedType(env);
};

const parseTermType = (env: Env): TypeNode => {
  const result = parseRootType(env);
  if (check(env, TT.Symbol, '[')) {
    const base = makeBaseNode(env);
    append(base, result);
    expect(env, 'Expected: [', base, TT.Symbol, '[');
    expect(env, 'Expected: ]', base, TT.Symbol, ']');
    return {base, kind: NT.ArrayType, element: result};
  }
  return result;
};

const parseType = (env: Env): TypeNode => {
  const result = parseTermType(env);
  if (!check(env, TT.Symbol, '|')) return result;

  const base = makeBaseNode(env);
  const options = [result];
  append(base, result);
  expect(env, 'Expected: |', base, TT.Symbol, '|');
  do {
    options.push(parseTermType(env));
    append(base, options[options.length - 1]!);
  } while (consume(env, base, TT.Symbol, '|'));
  return {base, kind: NT.UnionType, options};
};

//////////////////////////////////////////////////////////////////////////////

// Entry points

const formatAST = (input: string, root: Node): string => {
  const lines = [] as string[];
  const recurse = (node: Node, depth: int): void => {
    const base = node.base;
    const kind = NT[node.kind]!;
    const spacer = ' '.repeat(2 * depth);
    const suffix = base.text.length > 0 ? `: ${base.text}` : '';
    lines.push(`${spacer}${base.pos}:${base.end}:${kind}${suffix}`);
    for (const child of base.children) recurse(child, depth + 1);
  };
  recurse(root, 0);
  return lines.join('\n');
}

const formatDiagnostics =
    (input: string, diagnostics: Diagnostic[], verbose: boolean): string => {
  const full = input.length;
  const size = full > 0 && input[full - 1] === '\n' ? full - 1 : full;
  const sorted = diagnostics.slice().sort((a, b) => a.pos - b.pos);
  let linePos = -1;
  let lineEnd = -1;
  let line = 0;

  let prev = -1;
  const lines = [] as string[];
  for (const diagnostic of sorted) {
    const pos = Math.min(diagnostic.pos, size);
    while (lineEnd < pos) {
      line++;
      linePos = lineEnd;
      for (lineEnd++; lineEnd < size && input[lineEnd]! !== '\n'; lineEnd++) {}
    }
    lines.push(`${line}:${pos - linePos}:${diagnostic.error}`);
    if (verbose) {
      lines.push(`  ${input.substring(linePos + 1, lineEnd)}`);
      lines.push(`  ${' '.repeat(pos - linePos - 1)}^`);
    }
  }
  return lines.join('\n');
}

const parseProgram = (input: string, tokens: Token[],
                      diagnostics: Diagnostic[]): TypeNode => {
  const env = {input, tokens, diagnostics, i: 0};
  return parseType(env);
};

declare const console: {log: any};
declare const process: {argv: string[]};
declare const require: (x: string) => any;

const main = () => {
  const fs = require('fs');
  const args = (process.argv as string[]).slice();
  if (!(args.length === 3 || (args.length === 4 && args[3] === '-v'))) {
    throw Error(`Usage: node parser.js $FILE [-v]`);
  }
  const verbose = args.length > 3;
  const input = fs.readFileSync(args[2], 'utf8');
  const diagnostics = [] as Diagnostic[];
  const tokens = lex(input, diagnostics);
  const program = parseProgram(input, tokens, diagnostics);
  if (verbose) console.log(formatAST(input, program));
  if (verbose && diagnostics.length > 0) console.log();
  console.log(formatDiagnostics(input, diagnostics, true));
};

main();
