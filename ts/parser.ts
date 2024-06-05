type int = number;

const assert = (x: boolean): void => { if (!x) throw new Error(); };

//////////////////////////////////////////////////////////////////////////////

// TokenSet

type TokenSet = {
  prefixes: Map<string, boolean>,
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
  'as', 'break', 'const', 'do', 'continue', 'class', 'declare', 'else',
  'enum', 'export', 'extends', 'for', 'if', 'import', 'private', 'let',
  'new', 'of', 'return', 'throw', 'while', 'void', 'true', 'false',
]);

const ops = makeTokenSet([
  '+', '+=', '-', '-=', '*', '*=', '/', '/=', '%', '**',
  '<', '<=', '>', '>=', '==', '!=', '===', '!==', '=',
  '(', ')', '[', ']', '{', '}', '=>', '?', ':', '.', ',', ';',
  '!', '~', '|', '&', '^', '&&', '||', '??', '<<', '>>', '++', '--',
]);

// Lower precedence means tighter binding.
//
// Precedence is a range: it is ambiguous for ops like bitwise ops, etc.
// We will force the user to place parentheses in an expr like a + b | c.
//
// Some ops support repetition. The ops that do are all left-associative.
type Precedence = {
  glb: int,
  lub: int,
  repeat: boolean,
};

const assignment = new Set(['=', '+=', '-=', '*=', '/=']);

const preops = new Set(['!', '~', '+', '-', '++', '--']);

const postop = new Set(['++', '--']);

const binops = new Map([
  ['**',  {glb: 1, lub: 1, repeat: false}],
  ['*',   {glb: 2, lub: 2, repeat: true}],
  ['/',   {glb: 2, lub: 2, repeat: true}],
  ['+',   {glb: 3, lub: 3, repeat: true}],
  ['-',   {glb: 3, lub: 3, repeat: true}],
  // Bit-ops with confusing precedence.
  ['%',   {glb: 0, lub: 4, repeat: false}],
  ['<<',  {glb: 0, lub: 4, repeat: false}],
  ['>>',  {glb: 0, lub: 4, repeat: false}],
  // Comparisons and equality ops.
  ['<',   {glb: 5, lub: 5, repeat: false}],
  ['<=',  {glb: 5, lub: 5, repeat: false}],
  ['>',   {glb: 5, lub: 5, repeat: false}],
  ['>=',  {glb: 5, lub: 5, repeat: false}],
  ['==',  {glb: 6, lub: 6, repeat: false}],
  ['!=',  {glb: 6, lub: 6, repeat: false}],
  ['===', {glb: 6, lub: 6, repeat: false}],
  ['!==', {glb: 6, lub: 6, repeat: false}],
  ['&&',  {glb: 7, lub: 8, repeat: true}],
  ['||',  {glb: 7, lub: 8, repeat: true}],
  // Bit-ops with confusing precedence.
  ['&',   {glb: 0, lub: 9, repeat: true}],
  ['|',   {glb: 0, lub: 9, repeat: true}],
  ['^',   {glb: 0, lub: 9, repeat: true}],
  ['??',  {glb: 0, lub: 9, repeat: true}],
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

type Diagnostic = {
  pos: int,
  error: string,
};

type Token = {
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
  ArgDefinition,
  ArgType,
  CallArgs,
  FieldExpr,
  FieldType,
  CondClause,
  Program,
  // Types
  ArrayType,
  ErrorType,
  TupleType,
  UnionType,
  ValueType,
  StructType,
  ClosureType,
  GenericType,
  IdentifierType,
  // Expressions
  ErrorExpr,
  BinaryOpExpr,
  UnaryPrefixOpExpr,
  UnarySuffixOpExpr,
  CastExpr,
  ArrayExpr,
  StructExpr,
  ClosureExpr,
  TernaryExpr,
  TemplateExpr,
  AssignmentExpr,
  FieldAccessExpr,
  IndexAccessExpr,
  FunctionCallExpr,
  ConstructorCallExpr,
  IdentifierExpr,
  DblLiteralExpr,
  IntLiteralExpr,
  StrLiteralExpr,
  BoolLiteralExpr,
  NullLiteralExpr,
  // Statements
  IfStatement,
  ExprStatement,
  BlockStatement,
  EmptyStatement,
  ThrowStatement,
  ForEachStatement,
  ForLoopStatement,
  WhileLoopStatement,
  DoWhileLoopStatement,
  BreakStatement,
  ContinueStatement,
  ReturnStatement,
  DeclarationStatement,
  EnumDeclarationStatement,
  TypeDeclarationStatement,
  ExternDeclarationStatement,
};

type BaseNode = {
  pos: int,
  end: int,
  text: string,
  children: Node[],
};

type IdentifierNode = {base: BaseNode, kind: NT.Identifier};
type KeywordNode    = {base: BaseNode, kind: NT.Keyword};
type OperatorNode   = {base: BaseNode, kind: NT.Operator};
type TemplateNode   = {base: BaseNode, kind: NT.Template};

type ArgDefinitionNode =
    {base: BaseNode, kind: NT.ArgDefinition,
     name: IdentifierNode, type: TypeNode, def: ExprNode | null};
type ArgTypeNode =
    {base: BaseNode, kind: NT.ArgType,
     name: IdentifierNode, type: TypeNode, opt: OperatorNode | null};
type CallArgsNode =
    {base: BaseNode, kind: NT.CallArgs, args: ExprNode[]};
type FieldExprNode =
    {base: BaseNode, kind: NT.FieldExpr, name: IdentifierNode, expr: ExprNode};
type FieldTypeNode =
    {base: BaseNode, kind: NT.FieldType, name: IdentifierNode, type: TypeNode};
type CondClauseNode =
    {base: BaseNode, kind: NT.CondClause, cond: ExprNode, then: StatementNode};
type ProgramNode =
    {base: BaseNode, kind: NT.Program, statements: StatementNode[]};

type ArrayTypeNode =
    {base: BaseNode, kind: NT.ArrayType, element: TypeNode};
type ErrorTypeNode =
    {base: BaseNode, kind: NT.ErrorType};
type TupleTypeNode =
    {base: BaseNode, kind: NT.TupleType, elements: TypeNode[]};
type UnionTypeNode =
    {base: BaseNode, kind: NT.UnionType, options: TypeNode[]};
type ValueTypeNode =
    {base: BaseNode, kind: NT.ValueType, root: IdentifierNode, field: IdentifierNode};
type StructTypeNode =
    {base: BaseNode, kind: NT.StructType, items: FieldTypeNode[]};
type ClosureTypeNode =
    {base: BaseNode, kind: NT.ClosureType, args: ArgTypeNode[], result: TypeNode};
type GenericTypeNode =
    {base: BaseNode, kind: NT.GenericType, name: IdentifierNode, args: TypeNode[]};
type IdentifierTypeNode =
    {base: BaseNode, kind: NT.IdentifierType};

type ErrorExprNode =
    {base: BaseNode, kind: NT.ErrorExpr};
type BinaryOpExprNode =
    {base: BaseNode, kind: NT.BinaryOpExpr,
     op: OperatorNode, lhs: ExprNode, rhs: ExprNode};
type UnaryPrefixOpExprNode =
    {base: BaseNode, kind: NT.UnaryPrefixOpExpr, op: OperatorNode, expr: ExprNode};
type UnarySuffixOpExprNode =
    {base: BaseNode, kind: NT.UnarySuffixOpExpr, op: OperatorNode, expr: ExprNode};
type CastExprNode =
    {base: BaseNode, kind: NT.CastExpr, expr: ExprNode, type: TypeNode};
type ArrayExprNode =
    {base: BaseNode, kind: NT.ArrayExpr, elements: ExprNode[]};
type StructExprNode =
    {base: BaseNode, kind: NT.StructExpr, items: FieldExprNode[]};
type ClosureExprNode =
    {base: BaseNode, kind: NT.ClosureExpr,
     args: ArgDefinitionNode[], result: TypeNode, body: BlockStatementNode};
type TernaryExprNode =
    {base: BaseNode, kind: NT.TernaryExpr,
     cond: ExprNode, lhs: ExprNode, rhs: ExprNode};
type TemplateExprNode =
    {base: BaseNode, kind: NT.TemplateExpr,
     prefix: TemplateNode, suffixes: [ExprNode, TemplateNode][]};
type AssignmentExprNode =
    {base: BaseNode, kind: NT.AssignmentExpr,
     op: OperatorNode, lhs: ExprNode, rhs: ExprNode};
type FieldAccessExprNode =
    {base: BaseNode, kind: NT.FieldAccessExpr, root: ExprNode, field: IdentifierNode};
type IndexAccessExprNode =
    {base: BaseNode, kind: NT.IndexAccessExpr, root: ExprNode, index: ExprNode};
type FunctionCallExprNode =
    {base: BaseNode, kind: NT.FunctionCallExpr, fn: ExprNode, args: CallArgsNode};
type ConstructorCallExprNode =
    {base: BaseNode, kind: NT.ConstructorCallExpr,
     cls: IdentifierNode, args: CallArgsNode};

type IdentifierExprNode  = {base: BaseNode, kind: NT.IdentifierExpr};
type DblLiteralExprNode  = {base: BaseNode, kind: NT.DblLiteralExpr};
type IntLiteralExprNode  = {base: BaseNode, kind: NT.IntLiteralExpr};
type StrLiteralExprNode  = {base: BaseNode, kind: NT.StrLiteralExpr};
type BoolLiteralExprNode = {base: BaseNode, kind: NT.BoolLiteralExpr};
type NullLiteralExprNode = {base: BaseNode, kind: NT.NullLiteralExpr};

type IfStatementNode =
    {base: BaseNode, kind: NT.IfStatement,
     cases: CondClauseNode[], elseCase: StatementNode | null};
type ExprStatementNode =
    {base: BaseNode, kind: NT.ExprStatement, expr: ExprNode};
type BlockStatementNode =
    {base: BaseNode, kind: NT.BlockStatement, statements: StatementNode[]};
type EmptyStatementNode =
    {base: BaseNode, kind: NT.EmptyStatement};
type ThrowStatementNode =
    {base: BaseNode, kind: NT.ThrowStatement, expr: ExprNode};
type ForEachStatementNode =
    {base: BaseNode, kind: NT.ForEachStatement,
     keyword: KeywordNode, name: IdentifierNode, expr: ExprNode, body: StatementNode};
type ForLoopStatementNode =
    {base: BaseNode, kind: NT.ForLoopStatement,
     init: StatementNode, cond: ExprNode, post: StatementNode, body: StatementNode};
type WhileLoopStatementNode =
    {base: BaseNode, kind: NT.WhileLoopStatement,
     cond: ExprNode, body: StatementNode};
type DoWhileLoopStatementNode =
    {base: BaseNode, kind: NT.DoWhileLoopStatement,
     cond: ExprNode, body: StatementNode};
type BreakStatementNode =
    {base: BaseNode, kind: NT.BreakStatement};
type ContinueStatementNode =
    {base: BaseNode, kind: NT.ContinueStatement};
type ReturnStatementNode =
    {base: BaseNode, kind: NT.ReturnStatement, expr: ExprNode | null};
type DeclarationStatementNode =
    {base: BaseNode, kind: NT.DeclarationStatement,
     keyword: KeywordNode, name: IdentifierNode, expr: ExprNode};
type EnumDeclarationStatementNode =
    {base: BaseNode, kind: NT.EnumDeclarationStatement,
     name: IdentifierNode, options: IdentifierNode[]};
type TypeDeclarationStatementNode =
    {base: BaseNode, kind: NT.TypeDeclarationStatement,
     name: IdentifierNode, type: TypeNode};
type ExternDeclarationStatementNode =
    {base: BaseNode, kind: NT.ExternDeclarationStatement,
     name: IdentifierNode, type: TypeNode};

type TypeNode =
    ArrayTypeNode |
    ErrorTypeNode |
    TupleTypeNode |
    UnionTypeNode |
    ValueTypeNode |
    StructTypeNode |
    ClosureTypeNode |
    GenericTypeNode |
    IdentifierTypeNode;

type ExprNode =
    ErrorExprNode |
    BinaryOpExprNode |
    UnaryPrefixOpExprNode |
    UnarySuffixOpExprNode |
    CastExprNode |
    ArrayExprNode |
    StructExprNode |
    ClosureExprNode |
    TernaryExprNode |
    TemplateExprNode |
    AssignmentExprNode |
    FieldAccessExprNode |
    IndexAccessExprNode |
    FunctionCallExprNode |
    ConstructorCallExprNode |
    IdentifierExprNode |
    DblLiteralExprNode |
    IntLiteralExprNode |
    StrLiteralExprNode |
    BoolLiteralExprNode |
    NullLiteralExprNode;

type StatementNode =
    IfStatementNode |
    ExprStatementNode |
    BlockStatementNode |
    EmptyStatementNode |
    ThrowStatementNode |
    ForEachStatementNode |
    ForLoopStatementNode |
    WhileLoopStatementNode |
    DoWhileLoopStatementNode |
    BreakStatementNode |
    ContinueStatementNode |
    ReturnStatementNode |
    DeclarationStatementNode |
    EnumDeclarationStatementNode |
    TypeDeclarationStatementNode |
    ExternDeclarationStatementNode;

type Node =
    IdentifierNode |
    KeywordNode |
    OperatorNode |
    TemplateNode |
    ArgDefinitionNode |
    ArgTypeNode |
    CallArgsNode |
    FieldExprNode |
    FieldTypeNode |
    CondClauseNode |
    ProgramNode |
    TypeNode |
    ExprNode |
    StatementNode;

//////////////////////////////////////////////////////////////////////////////

// Parsing

type Env = {
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

const append = (node: BaseNode, child: Node): void => {
  if (child.base.pos !== child.base.end) {
    node.pos = Math.min(node.pos, child.base.pos);
    node.end = Math.max(node.end, child.base.end);
  }
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
  assert(env.i < env.tokens.length);
  const token = env.tokens[env.i++]!;
  if (node) includeToken(node, token);
  return true;
};

const expect = (env: Env, error: string, node: BaseNode | null,
                type: TT, text: string | null = null): boolean => {
  if (consume(env, node, type, text)) return true;
  env.diagnostics.push({pos: cursor(env), error});
  return false;
};

const includeToken = (node: BaseNode, token: Token): void => {
  node.pos = Math.min(node.pos, token.pos);
  node.end = Math.max(node.pos, token.end);
};

const makeBaseNode = (env: Env): BaseNode => {
  const i = env.i;
  const tokens = env.tokens;
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

const makeTokenNode = (env: Env): BaseNode => {
  const base = makeBaseNode(env);
  const token = env.tokens[env.i++]!;
  includeToken(base, token);
  base.text = token.text;
  return base;
};

// Basic nodes

const curTokenText = (env: Env, error: string, base: BaseNode,
                      type: TT, alt: string): string => {
  return expect(env, error, base, type) ? env.tokens[env.i - 1]!.text : alt;
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

const parseKeywordIdentifier = (env: Env): KeywordNode => {
  const base = makeBaseNode(env);
  base.text = curTokenText(env, 'Expected: keyword', base, TT.Identifier, '');
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

const parseArgType = (env: Env, alt: string): ArgTypeNode => {
  const base = makeBaseNode(env);
  const name = parseIdentifier(env, alt);
  append(base, name);
  const opt = check(env, TT.Symbol, '?') ? parseOperator(env) : null;
  if (opt) append(base, opt);
  expect(env, 'Expected: :', base, TT.Symbol, ':');
  const type = parseType(env);
  append(base, type);
  return {base, kind: NT.ArgType, name, type, opt};
};

const parseFieldType = (env: Env, alt: string): FieldTypeNode => {
  const base = makeBaseNode(env);
  const name = parseIdentifier(env, alt);
  append(base, name);
  expect(env, 'Expected: :', base, TT.Symbol, ':');
  const type = parseType(env);
  append(base, type);
  return {base, kind: NT.FieldType, name, type};
};

const parseClosureType = (env: Env): ClosureTypeNode | null => {
  const parseBody = (base: BaseNode, args: ArgTypeNode[]): ClosureTypeNode => {
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
           ahead(env, i + 1, TT.Symbol, '?') ||
           checkForArrow(i + 1);
  };

  if (check(env, TT.Symbol, '(') && checkForArgList(1)) {
    const base = makeBaseNode(env);
    const args = [] as ArgTypeNode[];
    expect(env, 'Expected: (', base, TT.Symbol, '(');
    if (consume(env, base, TT.Symbol, ')')) return parseBody(base, args);
    do {
      args.push(parseArgType(env, `$${args.length}`));
      append(base, args[args.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, ')'));
    expect(env, 'Expected: )', base, TT.Symbol, ')');
    return parseBody(base, args);
  }
  return null;
};

const parseQualifiedType = (env: Env): TypeNode => {
  if (check(env, TT.NullLiteral) || check(env, TT.VoidLiteral)) {
    return {base: makeTokenNode(env), kind: NT.IdentifierType};
  }

  if (!check(env, TT.Identifier)) {
    env.diagnostics.push({pos: cursor(env), error: 'Expected: type'});
    return {base: makeBaseNode(env), kind: NT.ErrorType};
  } else if (ahead(env, 1, TT.Symbol, '.')) {
    const base = makeBaseNode(env);
    const root = parseIdentifier(env, '$enum');
    append(base, root);
    expect(env, 'Expected: .', base, TT.Symbol, '.');
    const field = parseIdentifier(env, '$option');
    append(base, field);
    return {base, kind: NT.ValueType, root, field};
  } else if (!ahead(env, 1, TT.Symbol, '<')) {
    return parseIdentifierType(env, '');
  }

  const base = makeBaseNode(env);
  const name = parseIdentifier(env, '');
  append(base, name);
  const args = [] as TypeNode[];
  expect(env, 'Expected: <', base, TT.Symbol, '<');
  do {
    args.push(parseType(env));
    append(base, args[args.length - 1]!);
  } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, '>'));
  expect(env, 'Expected: >', base, TT.Symbol, '>');
  return {base, kind: NT.GenericType, name, args};
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
    expect(env, 'Expected: {', base, TT.Symbol, '{');
    const items = [] as FieldTypeNode[];
    if (consume(env, base, TT.Symbol, '}')) {
      return {base, kind: NT.StructType, items};
    }
    do {
      items.push(parseFieldType(env, 'Item'));
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

// Expression grammar

const parseArgDefinition = (env: Env, alt: string): ArgDefinitionNode => {
  const base = makeBaseNode(env);
  const name = parseIdentifier(env, alt);
  append(base, name);
  expect(env, 'Expected: :', base, TT.Symbol, ':');
  const type = parseType(env);
  append(base, type);
  const def = consume(env, base, TT.Symbol, '=') ? parseExpr(env) : null;
  if (def) append(base, def);
  return {base, kind: NT.ArgDefinition, name, type, def};
};

const parseCallArgs = (env: Env): CallArgsNode => {
  const base = makeBaseNode(env);
  const args = [] as ExprNode[];
  if (check(env, TT.Symbol, ')')) return {base, kind: NT.CallArgs, args};
  do {
    args.push(parseExpr(env));
    append(base, args[args.length - 1]!);
  } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, ')'));
  return {base, kind: NT.CallArgs, args};
};

const parseFieldExpr = (env: Env, alt: string): FieldExprNode => {
  const base = makeBaseNode(env);
  const name = parseIdentifier(env, alt);
  append(base, name);
  const expr = ((): ExprNode => {
    if (consume(env, base, TT.Symbol, ':')) return parseExpr(env);
    const prev = name.base;
    const copy = {pos: prev.pos, end: prev.end, text: prev.text, children: []};
    return {base: copy, kind: NT.IdentifierExpr};
  })();
  append(base, expr);
  return {base, kind: NT.FieldExpr, name, expr};
};

const parseFunctionBody = (env: Env): BlockStatementNode => {
  const block = parseBlockStatement(env);
  if (block) return block;

  const base = makeBaseNode(env);
  const statement = ((): ReturnStatementNode => {
    const base = makeBaseNode(env);
    const expr = parseExpr(env);
    append(base, expr);
    return {base, kind: NT.ReturnStatement, expr};
  })();
  append(base, statement);
  return {base, kind: NT.BlockStatement, statements: [statement]};
};

const parseClosureExpr = (env: Env): ClosureExprNode | null => {
  const parseBody = (base: BaseNode, args: ArgDefinitionNode[]): ClosureExprNode => {
    expect(env, 'Expected: :', base, TT.Symbol, ':');
    const result = parseType(env);
    append(base, result);
    expect(env, 'Expected: =>', base, TT.Symbol, '=>');
    const body = parseFunctionBody(env);
    append(base, body);
    return {base, kind: NT.ClosureExpr, args, result, body};
  };

  const checkForArrow = (i: int): boolean => {
    if (!ahead(env, i, TT.Symbol, ')')) return false;
    return ahead(env, i + 1, TT.Symbol, '=>') ||
           ahead(env, i + 1, TT.Symbol, ':');
  };

  const checkForArgList = (i: int): boolean => {
    if (checkForArrow(i)) return true;
    if (!ahead(env, i, TT.Identifier)) return false;
    return ahead(env, i + 1, TT.Symbol, ',') ||
           ahead(env, i + 1, TT.Symbol, ':') ||
           ahead(env, i + 1, TT.Symbol, '=') ||
           checkForArrow(i + 1);
  };

  if (check(env, TT.Identifier) && ahead(env, 1, TT.Symbol, '=>')) {
    const base = makeBaseNode(env);
    const args = [parseArgDefinition(env, '$0')];
    append(base, args[args.length - 1]!);
    return parseBody(base, args);
  }

  if (check(env, TT.Symbol, '(') && checkForArgList(1)) {
    const base = makeBaseNode(env);
    const args = [] as ArgDefinitionNode[];
    expect(env, 'Expected: (', base, TT.Symbol, '(');
    if (consume(env, base, TT.Symbol, ')')) return parseBody(base, args);
    do {
      args.push(parseArgDefinition(env, `$${args.length}`));
      append(base, args[args.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, ')'));
    expect(env, 'Expected: )', base, TT.Symbol, ')');
    return parseBody(base, args);
  }
  return null;
};

const parseConstructorCallExpr = (env: Env): ConstructorCallExprNode | null => {
  if (!check(env, TT.Keyword, 'new')) return null;
  const base = makeBaseNode(env);
  expect(env, 'Expected: new', base, TT.Keyword, 'new');
  const cls = parseIdentifier(env, '$class');
  append(base, cls);
  expect(env, 'Expected: (', base, TT.Symbol, '(');
  const args = parseCallArgs(env);
  append(base, args);
  expect(env, 'Expected: )', null, TT.Symbol, ')');
  return {base, kind: NT.ConstructorCallExpr, cls, args};
};

const parseRootExpr = (env: Env): ExprNode => {
  const token = (): BaseNode => makeTokenNode(env);
  if (check(env, TT.Identifier))  return {base: token(), kind: NT.IdentifierExpr};
  if (check(env, TT.DblLiteral))  return {base: token(), kind: NT.DblLiteralExpr};
  if (check(env, TT.IntLiteral))  return {base: token(), kind: NT.IntLiteralExpr};
  if (check(env, TT.StrLiteral))  return {base: token(), kind: NT.StrLiteralExpr};
  if (check(env, TT.BoolLiteral)) return {base: token(), kind: NT.BoolLiteralExpr};
  if (check(env, TT.NullLiteral)) return {base: token(), kind: NT.NullLiteralExpr};

  if (consume(env, null, TT.Symbol, '(')) {
    const result = parseExpr(env);
    expect(env, 'Expected: )', null, TT.Symbol, ')');
    return result;
  }

  if (check(env, TT.Symbol, '[')) {
    const base = makeBaseNode(env);
    expect(env, 'Expected: [', base, TT.Symbol, '[');
    const elements = [] as ExprNode[];
    if (consume(env, base, TT.Symbol, ']')) {
      return {base, kind: NT.ArrayExpr, elements};
    }
    do {
      elements.push(parseExpr(env));
      append(base, elements[elements.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, ']'));
    expect(env, 'Expected: ]', base, TT.Symbol, ']');
    return {base, kind: NT.ArrayExpr, elements};
  }

  if (check(env, TT.Symbol, '{')) {
    const base = makeBaseNode(env);
    expect(env, 'Expected: {', base, TT.Symbol, '{');
    const items = [] as FieldExprNode[];
    if (consume(env, base, TT.Symbol, '}')) {
      return {base, kind: NT.StructExpr, items};
    }
    do {
      items.push(parseFieldExpr(env, '$field'));
      append(base, items[items.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, '}'));
    expect(env, 'Expected: }', base, TT.Symbol, '}');
    return {base, kind: NT.StructExpr, items};
  }

  if (check(env, TT.TemplateStart)) {
    const base = makeBaseNode(env);
    const prefix = {base: token(), kind: NT.Template} as TemplateNode;
    const suffixes = [] as [ExprNode, TemplateNode][];
    append(base, prefix);
    while (true) {
      const before = env.i;
      const expr = parseExpr(env);
      append(base, expr);
      if (check(env, TT.TemplateEnd)) {
        const template = {base: token(), kind: NT.Template} as TemplateNode;
        suffixes.push([expr, template]);
        append(base, template);
        break;
      }
      if (expect(env, 'Expected: template', base, TT.TemplateMid)) env.i--;
      const template = {base: token(), kind: NT.Template} as TemplateNode;
      suffixes.push([expr, template]);
      append(base, template);
      if (env.i === before && !advance(env)) break;
    }
    return {base, kind: NT.TemplateExpr, prefix, suffixes};
  }

  env.diagnostics.push({pos: cursor(env), error: 'Expected: expression'});
  return {base: makeBaseNode(env), kind: NT.ErrorExpr};
};

const parseUnaryOpTerm = (env: Env): ExprNode => {
  let expr = parseRootExpr(env);
  while (true) {
    if (!check(env, TT.Symbol)) break;
    const symbol = env.tokens[env.i]!.text;

    if (symbol === '(') {
      const base = makeBaseNode(env);
      append(base, expr);
      expect(env, 'Expected: (', base, TT.Symbol, '(');
      const args = parseCallArgs(env);
      append(base, args);
      expect(env, 'Expected: )', null, TT.Symbol, ')');
      expr = {base, kind: NT.FunctionCallExpr, fn: expr, args};
      continue;
    }

    if (symbol === '.') {
      const base = makeBaseNode(env);
      append(base, expr);
      expect(env, 'Expected: .', base, TT.Symbol, '.');
      const field = parseIdentifier(env, '$field');
      append(base, field);
      expr = {base, kind: NT.FieldAccessExpr, root: expr, field};
      continue;
    }

    if (symbol === '[') {
      const base = makeBaseNode(env);
      append(base, expr);
      expect(env, 'Expected: [', base, TT.Symbol, '[');
      const index = parseExpr(env);
      append(base, index);
      expect(env, 'Expected: ]', base, TT.Symbol, ']');
      expr = {base, kind: NT.IndexAccessExpr, root: expr, index};
      continue;
    }

    if (symbol === '!') {
      const base = makeBaseNode(env);
      append(base, expr);
      const op = parseOperator(env);
      append(base, op);
      expr = {base, kind: NT.UnarySuffixOpExpr, op, expr};
      continue;
    }
    break;
  }
  return expr;
};

const parseUnaryOpExpr = (env: Env): ExprNode => {
  const pre = [] as OperatorNode[];

  while (check(env, TT.Symbol)) {
    const symbol = env.tokens[env.i]!.text;
    if (!preops.has(symbol)) break;
    pre.push(parseOperator(env));
  }

  let expr = parseUnaryOpTerm(env);

  while (check(env, TT.Symbol)) {
    const symbol = env.tokens[env.i]!.text;
    if (!postop.has(symbol)) break;
    const base = makeBaseNode(env);
    append(base, expr);
    const op = parseOperator(env);
    append(base, op);
    expr = {base, kind: NT.UnarySuffixOpExpr, op, expr};
  }

  while (pre.length > 0) {
    const base = makeBaseNode(env);
    const op = pre.pop()!;
    append(base, op);
    append(base, expr);
    expr = {base, kind: NT.UnaryPrefixOpExpr, op, expr};
  }
  return expr;
};

const parseBinaryOpTerm = (env: Env): ExprNode => {
  const expr = parseUnaryOpExpr(env);
  if (!check(env, TT.Keyword, 'as')) return expr;

  const base = makeBaseNode(env);
  append(base, expr);
  expect(env, 'Expected: as', base, TT.Keyword, 'as');
  const type = parseType(env);
  append(base, type);
  return {base, kind: NT.CastExpr, expr, type};
};

const parseBinaryOpExpr = (env: Env): ExprNode => {
  const terms = [] as ExprNode[];
  const ops = [] as [OperatorNode, Precedence][];
  terms.push(parseBinaryOpTerm(env));

  const evalOneOp = (): void => {
    const rhs = terms.pop()!;
    const lhs = terms.pop()!;
    const op = ops.pop()![0];
    const base = makeBaseNode(env);
    append(base, lhs);
    append(base, op);
    append(base, rhs);
    terms.push({base, kind: NT.BinaryOpExpr, op, lhs, rhs});
  };

  while (true) {
    if (!check(env, TT.Symbol)) break;
    const pos = cursor(env);
    const symbol = env.tokens[env.i]!.text;
    const precedence = binops.get(symbol) ?? null;
    if (precedence === null) break;

    const next = [parseOperator(env), precedence] as [OperatorNode, Precedence];
    while (ops.length > 0 && ops[ops.length - 1]![1].glb <= next[1].lub) {
      const prev = ops[ops.length - 1]!;
      const prevOp = prev[0].base.text;
      const nextOp = next[0].base.text;
      if (prevOp === nextOp || prev[1].glb === next[1].lub) {
        if (!next[1].repeat) {
          const error = `Non-associative op: ${nextOp}`;
          env.diagnostics.push({pos, error});
        }
      } else if (prev[1].lub >= next[1].glb) {
        const error = `Ambiguous precedence: ${prevOp} vs. ${nextOp}`;
        env.diagnostics.push({pos, error});
      }
      evalOneOp();
    }
    ops.push(next);
    terms.push(parseUnaryOpExpr(env));
  }

  while (ops.length > 0) evalOneOp();
  assert(terms.length === 1);
  return terms.pop()!;
};

const parseExpr = (env: Env): ExprNode => {
  const fn = parseClosureExpr(env);
  if (fn) return fn;
  const ct = parseConstructorCallExpr(env);
  if (ct) return ct;

  const expr = parseBinaryOpExpr(env);
  if (!check(env, TT.Symbol)) return expr;

  const symbol = env.tokens[env.i]!.text;
  if (symbol === '?') {
    const base = makeBaseNode(env);
    append(base, expr);
    expect(env, 'Expected: ?', base, TT.Symbol, '?');
    const lhs = parseExpr(env);
    append(base, lhs);
    expect(env, 'Expected: :', base, TT.Symbol, ':');
    const rhs = parseExpr(env);
    append(base, rhs);
    return {base, kind: NT.TernaryExpr, cond: expr, lhs, rhs};
  }

  if (assignment.has(symbol)) {
    const base = makeBaseNode(env);
    append(base, expr);
    const op = parseOperator(env);
    append(base, op);
    const rhs = parseExpr(env);
    append(base, rhs);
    return {base, kind: NT.AssignmentExpr, op, lhs: expr, rhs};
  }
  return expr;
};

// Statement grammar

const parseTrivialStatement = (env: Env, error: string, text: string,
                               parent: BaseNode | null): StatementNode => {
  const base = makeBaseNode(env);
  if (consume(env, base, TT.Symbol, text)) {
    return {base, kind: NT.EmptyStatement};
  }
  const expr = parseExpr(env);
  append(base, expr);
  expect(env, error, parent ?? base, TT.Symbol, text);
  return {base, kind: NT.ExprStatement, expr};
};

const parseTrivialStatementAndSemiColon =
    (env: Env, parent: BaseNode | null = null): StatementNode => {
  return parseTrivialStatement(env, 'Expected: ;', ';', parent);
};

const parseTrivialStatementAndParen =
    (env: Env, parent: BaseNode | null = null): StatementNode => {
  return parseTrivialStatement(env, 'Expected: )', ')', parent);
};

const parseBlockStatement = (env: Env): BlockStatementNode | null => {
  if (!check(env, TT.Symbol, '{')) return null;

  const base = makeBaseNode(env);
  expect(env, 'Expected: {', base, TT.Symbol, '{');
  const statements = [] as StatementNode[];
  while (!consume(env, base, TT.Symbol, '}')) {
    const before = env.i;
    statements.push(parseStatement(env));
    append(base, statements[statements.length - 1]!);
    if (env.i === before && !advance(env)) break;
  }
  return {base, kind: NT.BlockStatement, statements};
};

const parseStatement = (env: Env): StatementNode => {
  const parseDeclaration = (): StatementNode => {
    const base = makeBaseNode(env);
    const keyword = parseKeyword(env);
    append(base, keyword);
    const name = parseIdentifier(env, '$lhs');
    append(base, name);
    expect(env, 'Expected: =', base, TT.Symbol, '=');
    const expr = parseExpr(env);
    append(base, expr);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, kind: NT.DeclarationStatement, keyword, name, expr};
  };

  const parseCond = (): CondClauseNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: (', base, TT.Symbol, '(');
    const cond = parseExpr(env);
    append(base, cond);
    expect(env, 'Expected: )', base, TT.Symbol, ')');
    const then = parseStatement(env);
    append(base, then);
    return {base, kind: NT.CondClause, cond, then};
  };

  const parseIfStatement = (): StatementNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: if', base, TT.Keyword, 'if');
    const cases = [] as CondClauseNode[];
    let elseCase = null as StatementNode | null;
    cases.push(parseCond());
    append(base, cases[cases.length - 1]!);
    while (!elseCase && consume(env, base, TT.Keyword, 'else')) {
      if (consume(env, base, TT.Keyword, 'if')) {
        cases.push(parseCond());
        append(base, cases[cases.length - 1]!);
      } else {
        elseCase = parseStatement(env);
        append(base, elseCase);
      }
    }
    return {base, kind: NT.IfStatement, cases, elseCase};
  };

  const parseForLoop = (): StatementNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: for', base, TT.Keyword, 'for');
    expect(env, 'Expected: (', base, TT.Symbol, '(');
    let init = null as StatementNode | null;

    if (check(env, TT.Keyword, 'const') || check(env, TT.Keyword, 'let')) {
      const keyword = parseKeyword(env);
      append(base, keyword);
      const name = parseIdentifier(env, '$iter');
      append(base, name);

      if (consume(env, base, TT.Keyword, 'of')) {
        const expr = parseExpr(env);
        append(base, expr);
        expect(env, 'Expected: )', base, TT.Symbol, ')');
        const body = parseStatement(env);
        append(base, body);
        return {base, kind: NT.ForEachStatement, keyword, name, expr, body};
      }

      const parent = base;
      init = ((): StatementNode => {
        const base = makeBaseNode(env);
        append(base, keyword);
        append(base, name);
        expect(env, 'Expected: =', base, TT.Symbol, '=');
        const expr = parseExpr(env);
        append(base, expr);
        expect(env, 'Expected: ;', parent, TT.Symbol, ';');
        return {base, kind: NT.DeclarationStatement, keyword, name, expr};
      })();
    } else {
      init = parseTrivialStatementAndSemiColon(env, base);
    }

    append(base, init!);
    const cond = parseExpr(env);
    append(base, cond);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    const post = parseTrivialStatementAndParen(env, base);
    append(base, post);
    const body = parseStatement(env);
    append(base, body);
    return {base, kind: NT.ForLoopStatement, init, cond, post, body};
  };

  const parseDoWhileLoop = (): StatementNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: do', base, TT.Keyword, 'do');
    const body = parseStatement(env);
    append(base, body);
    expect(env, 'Expected: while', base, TT.Keyword, 'while');
    expect(env, 'Expected: (', base, TT.Symbol, '(');
    const cond = parseExpr(env);
    append(base, cond);
    expect(env, 'Expected: )', base, TT.Symbol, ')');
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, kind: NT.DoWhileLoopStatement, cond, body};
  };

  const parseWhileLoop = (): StatementNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: while', base, TT.Keyword, 'while');
    expect(env, 'Expected: (', base, TT.Symbol, '(');
    const cond = parseExpr(env);
    append(base, cond);
    expect(env, 'Expected: )', base, TT.Symbol, ')');
    const body = parseStatement(env);
    append(base, body);
    return {base, kind: NT.WhileLoopStatement, cond, body};
  };

  const parseThrow = (): StatementNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: throw', base, TT.Keyword, 'throw');
    const expr = parseExpr(env);
    append(base, expr);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, kind: NT.ThrowStatement, expr};
  };

  const parseReturn = (): StatementNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: return', base, TT.Keyword, 'return');
    if (consume(env, base, TT.Symbol, ';')) {
      return {base, kind: NT.ReturnStatement, expr: null};
    }
    const expr = parseExpr(env);
    append(base, expr);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, kind: NT.ReturnStatement, expr};
  };

  const parseEnumDeclaration = (): StatementNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: enum', base, TT.Keyword, 'enum');
    const name = parseIdentifier(env, '$enum');
    append(base, name);
    expect(env, 'Expected: {', base, TT.Symbol, '{');
    const options = [] as IdentifierNode[];
    if (consume(env, base, TT.Symbol, '}')) {
      return {base, kind: NT.EnumDeclarationStatement, name, options};
    }
    do {
      options.push(parseIdentifier(env, `$${options.length}`));
      append(base, options[options.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, '}'));
    expect(env, 'Expected: }', base, TT.Symbol, '}');
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, kind: NT.EnumDeclarationStatement, name, options};
  };

  const parseTypeDeclaration = (): StatementNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: type', base, TT.Identifier, 'type');
    const name = parseIdentifier(env, '$type');
    append(base, name);
    expect(env, 'Expected: =', base, TT.Symbol, '=');
    const type = parseType(env);
    append(base, type);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, kind: NT.TypeDeclarationStatement, name, type};
  };

  const parseExternDeclaration = (): StatementNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: declare', base, TT.Keyword, 'declare');
    expect(env, 'Expected: const', base, TT.Keyword, 'const');
    const name = parseIdentifier(env, '$type');
    append(base, name);
    expect(env, 'Expected: :', base, TT.Symbol, ':');
    const type = parseType(env);
    append(base, type);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, kind: NT.ExternDeclarationStatement, name, type};
  };

  const token = (): BaseNode => makeTokenNode(env);

  if (check(env, TT.Keyword) ||
      (check(env, TT.Identifier, 'type') && ahead(env, 1, TT.Identifier))) {
    const keyword = env.tokens[env.i]!.text;
    if (keyword === 'const')     return parseDeclaration();
    if (keyword === 'let')       return parseDeclaration();
    if (keyword === 'if')        return parseIfStatement();
    if (keyword === 'for')       return parseForLoop();
    if (keyword === 'while')     return parseWhileLoop();
    if (keyword === 'do')        return parseDoWhileLoop();
    if (keyword === 'throw')     return parseThrow();
    if (keyword === 'return')    return parseReturn();
    if (keyword === 'break')     return {base: token(), kind: NT.BreakStatement};
    if (keyword === 'continue')  return {base: token(), kind: NT.ContinueStatement};
    if (keyword === 'enum')      return parseEnumDeclaration();
    if (keyword === 'type')      return parseTypeDeclaration();
    if (keyword === 'declare')   return parseExternDeclaration();
  }

  const block = parseBlockStatement(env);
  return block ?? parseTrivialStatementAndSemiColon(env);
};

const parseProgram = (env: Env): ProgramNode => {
  const base = makeBaseNode(env);
  const statements = [] as StatementNode[];
  while (env.i < env.tokens.length) {
    const before = env.i;
    statements.push(parseStatement(env));
    append(base, statements[statements.length - 1]!);
    if (env.i === before) advance(env);
  }
  return {base, kind: NT.Program, statements};
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
};

const formatDiagnostics =
    (input: string, diagnostics: Diagnostic[], verbose: boolean): string => {
  const full = input.length;
  const size = full > 0 && input[full - 1] === '\n' ? full - 1 : full;
  const sorted = diagnostics.slice().sort(
      (a: Diagnostic, b: Diagnostic): int => a.pos - b.pos);
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
};

declare const console: {log: any};
declare const process: {argv: string[]};
declare const require: (x: string) => any;

const main = (): void => {
  const fs = require('fs');
  const args = (process.argv as string[]).slice();
  if (!(args.length === 3 || (args.length === 4 && args[3] === '-v'))) {
    throw new Error(`Usage: node parser.js $FILE [-v]`);
  }
  const verbose = args.length > 3;
  const input = fs.readFileSync(args[2], 'utf8');
  const diagnostics = [] as Diagnostic[];
  const tokens = lex(input, diagnostics);
  const program = parseProgram({input, tokens, diagnostics, i: 0});
  if (verbose) console.log(formatAST(input, program));
  if (verbose && diagnostics.length > 0) console.log();
  console.log(formatDiagnostics(input, diagnostics, true));
};

main();
