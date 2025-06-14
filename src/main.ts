type int = number;

const assert = (x: boolean): void => { if (!x) throw new Error(); };

//////////////////////////////////////////////////////////////////////////////

// TokenSet

type TokenSet = {
  prefixes: Map<string, boolean>,
};

const makeTokenSet = (items: string[]): TokenSet => {
  const prefixes = new Map() as Map<string, boolean>;
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
  'as', 'break', 'case', 'const', 'continue', 'default', 'declare', 'do', 'else',
  'enum', 'export', 'extends', 'for', 'if', 'import', 'private', 'let', 'new',
  'of', 'return', 'switch', 'throw', 'while', 'null', 'void', 'true', 'false',
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

const assignment = new Set(['=', '+=', '-=', '*=', '/=']) as Set<string>;

const preops = new Set(['!', '~', '+', '-', '++', '--']) as Set<string>;

const postop = new Set(['++', '--']) as Set<string>;

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
]) as Map<string, Precedence>;

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
  tag: TT,
  pos: int,
  end: int,
  text: string,
};

const lex = (input: string, diagnostics: Diagnostic[]): Token[] => {
  let i = 0;
  const size = input.length;
  const result = [] as Token[];
  const braceStack = [] as boolean[];
  let symbol = null as string | null;

  const consume = (a: string): boolean => {
    const result = i < input.length && input[i]! === a;
    if (result) i++;
    return result;
  };

  const consumeAll = (a: string): boolean => {
    const limit = i + a.length;
    if (limit > input.length) return false;
    for (let j = 0; j < a.length; j++) {
      if (input[i + j] !== a[j]) return false;
    }
    i = limit;
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

  const makeToken = (tag: TT, pos: int, end: int): Token => {
    assert(pos < end);
    return {tag, pos, end, text: input.substring(pos, end)};
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
        for (; i < size; i++) if (consumeAll(limit)) break;
        if (i === size) diagnostics.push({pos: i, error: `Unterminated ${start} comment`});
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
      if (token.tag === TT.TemplateStart) braceStack.push(true);
      result.push(token);
      continue;
    } else if (ch === '{') {
      result.push(makeToken(TT.Symbol, pos, ++i));
      braceStack.push(false);
      continue;
    } else if (ch === '}' && braceStack.length > 0) {
      if (braceStack[braceStack.length - 1]!) {
        const token = parseTemplate(ch);
        if (token.tag === TT.TemplateEnd) braceStack.pop();
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
        token.tag = TT.BoolLiteral;
      } else if (text === 'null') {
        token.tag = TT.NullLiteral;
      } else if (text === 'void') {
        token.tag = TT.VoidLiteral;
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
  SwitchCase,
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
  VariableExpr,
  DblLiteralExpr,
  IntLiteralExpr,
  StrLiteralExpr,
  BoolLiteralExpr,
  NullLiteralExpr,
  // Statements
  IfStmt,
  ExprStmt,
  BlockStmt,
  EmptyStmt,
  ThrowStmt,
  SwitchStmt,
  ForEachStmt,
  ForLoopStmt,
  WhileLoopStmt,
  DoWhileLoopStmt,
  BreakStmt,
  ContinueStmt,
  ReturnStmt,
  EnumDeclStmt,
  TypeDeclStmt,
  ExternDeclStmt,
  VariableDeclStmt,
};

type BaseNode = {
  pos: int,
  end: int,
  text: string,
  children: Node[],
};

type IdentifierNode = {base: BaseNode, tag: NT.Identifier};
type KeywordNode    = {base: BaseNode, tag: NT.Keyword};
type OperatorNode   = {base: BaseNode, tag: NT.Operator};
type TemplateNode   = {base: BaseNode, tag: NT.Template};

type ArgDefinitionNode =
    {base: BaseNode, tag: NT.ArgDefinition,
     name: IdentifierNode, type: TypeNode, def: ExprNode | null};
type ArgTypeNode =
    {base: BaseNode, tag: NT.ArgType,
     name: IdentifierNode, type: TypeNode, opt: OperatorNode | null};
type CallArgsNode =
    {base: BaseNode, tag: NT.CallArgs, args: ExprNode[]};
type FieldExprNode =
    {base: BaseNode, tag: NT.FieldExpr, name: IdentifierNode, expr: ExprNode};
type FieldTypeNode =
    {base: BaseNode, tag: NT.FieldType, name: IdentifierNode, type: TypeNode};
type SwitchCaseNode =
    {base: BaseNode, tag: NT.SwitchCase, expr: ExprNode | null, then: StmtNode};
type ProgramNode =
    {base: BaseNode, tag: NT.Program, stmts: StmtNode[]};

type ArrayTypeNode =
    {base: BaseNode, tag: NT.ArrayType, element: TypeNode};
type ErrorTypeNode =
    {base: BaseNode, tag: NT.ErrorType};
type TupleTypeNode =
    {base: BaseNode, tag: NT.TupleType, elements: TypeNode[]};
type UnionTypeNode =
    {base: BaseNode, tag: NT.UnionType, options: TypeNode[]};
type ValueTypeNode =
    {base: BaseNode, tag: NT.ValueType, root: IdentifierNode, field: IdentifierNode};
type StructTypeNode =
    {base: BaseNode, tag: NT.StructType, fields: FieldTypeNode[]};
type ClosureTypeNode =
    {base: BaseNode, tag: NT.ClosureType, args: ArgTypeNode[], result: TypeNode};
type GenericTypeNode =
    {base: BaseNode, tag: NT.GenericType, name: IdentifierNode, args: TypeNode[]};
type IdentifierTypeNode =
    {base: BaseNode, tag: NT.IdentifierType};

type ErrorExprNode =
    {base: BaseNode, tag: NT.ErrorExpr};
type BinaryOpExprNode =
    {base: BaseNode, tag: NT.BinaryOpExpr, op: OperatorNode, lhs: ExprNode, rhs: ExprNode};
type UnaryPrefixOpExprNode =
    {base: BaseNode, tag: NT.UnaryPrefixOpExpr, op: OperatorNode, expr: ExprNode};
type UnarySuffixOpExprNode =
    {base: BaseNode, tag: NT.UnarySuffixOpExpr, op: OperatorNode, expr: ExprNode};
type CastExprNode =
    {base: BaseNode, tag: NT.CastExpr, expr: ExprNode, type: TypeNode};
type ArrayExprNode =
    {base: BaseNode, tag: NT.ArrayExpr, elements: ExprNode[]};
type StructExprNode =
    {base: BaseNode, tag: NT.StructExpr, fields: FieldExprNode[]};
type ClosureExprNode =
    {base: BaseNode, tag: NT.ClosureExpr,
     args: ArgDefinitionNode[], result: TypeNode, body: BlockStmtNode};
type TernaryExprNode =
    {base: BaseNode, tag: NT.TernaryExpr,
     cond: ExprNode, lhs: ExprNode, rhs: ExprNode};
type TemplateExprNode =
    {base: BaseNode, tag: NT.TemplateExpr,
     prefix: TemplateNode, suffixes: [ExprNode, TemplateNode][]};
type AssignmentExprNode =
    {base: BaseNode, tag: NT.AssignmentExpr,
     op: OperatorNode, lhs: ExprNode, rhs: ExprNode};
type FieldAccessExprNode =
    {base: BaseNode, tag: NT.FieldAccessExpr, root: ExprNode, field: IdentifierNode};
type IndexAccessExprNode =
    {base: BaseNode, tag: NT.IndexAccessExpr, root: ExprNode, index: ExprNode};
type FunctionCallExprNode =
    {base: BaseNode, tag: NT.FunctionCallExpr, fn: ExprNode, args: CallArgsNode};
type ConstructorCallExprNode =
    {base: BaseNode, tag: NT.ConstructorCallExpr, cls: IdentifierNode, args: CallArgsNode};

type VariableExprNode    = {base: BaseNode, tag: NT.VariableExpr};
type DblLiteralExprNode  = {base: BaseNode, tag: NT.DblLiteralExpr};
type IntLiteralExprNode  = {base: BaseNode, tag: NT.IntLiteralExpr};
type StrLiteralExprNode  = {base: BaseNode, tag: NT.StrLiteralExpr};
type BoolLiteralExprNode = {base: BaseNode, tag: NT.BoolLiteralExpr};
type NullLiteralExprNode = {base: BaseNode, tag: NT.NullLiteralExpr};

type IfStmtNode =
    {base: BaseNode, tag: NT.IfStmt, cond: ExprNode, then: StmtNode, else: StmtNode | null};
type ExprStmtNode =
    {base: BaseNode, tag: NT.ExprStmt, expr: ExprNode};
type BlockStmtNode =
    {base: BaseNode, tag: NT.BlockStmt, stmts: StmtNode[]};
type EmptyStmtNode =
    {base: BaseNode, tag: NT.EmptyStmt};
type ThrowStmtNode =
    {base: BaseNode, tag: NT.ThrowStmt, expr: ExprNode};
type SwitchStmtNode =
    {base: BaseNode, tag: NT.SwitchStmt, expr: ExprNode, cases: SwitchCaseNode[]};
type ForEachStmtNode =
    {base: BaseNode, tag: NT.ForEachStmt,
     keyword: KeywordNode, name: IdentifierNode, expr: ExprNode, body: StmtNode};
type ForLoopStmtNode =
    {base: BaseNode, tag: NT.ForLoopStmt,
     init: StmtNode, cond: ExprStmtNode, post: StmtNode, body: StmtNode};
type WhileLoopStmtNode =
    {base: BaseNode, tag: NT.WhileLoopStmt, cond: ExprNode, body: StmtNode};
type DoWhileLoopStmtNode =
    {base: BaseNode, tag: NT.DoWhileLoopStmt, cond: ExprNode, body: StmtNode};
type BreakStmtNode =
    {base: BaseNode, tag: NT.BreakStmt};
type ContinueStmtNode =
    {base: BaseNode, tag: NT.ContinueStmt};
type ReturnStmtNode =
    {base: BaseNode, tag: NT.ReturnStmt, expr: ExprNode | null};
type VariableDeclStmtNode =
    {base: BaseNode, tag: NT.VariableDeclStmt,
     keyword: KeywordNode, name: IdentifierNode, expr: ExprNode};
type EnumDeclStmtNode =
    {base: BaseNode, tag: NT.EnumDeclStmt, name: IdentifierNode, values: IdentifierNode[]};
type TypeDeclStmtNode =
    {base: BaseNode, tag: NT.TypeDeclStmt, name: IdentifierNode, type: TypeNode};
type ExternDeclStmtNode =
    {base: BaseNode, tag: NT.ExternDeclStmt, name: IdentifierNode, type: TypeNode};

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
    VariableExprNode |
    DblLiteralExprNode |
    IntLiteralExprNode |
    StrLiteralExprNode |
    BoolLiteralExprNode |
    NullLiteralExprNode;

type StmtNode =
    IfStmtNode |
    ExprStmtNode |
    BlockStmtNode |
    EmptyStmtNode |
    ThrowStmtNode |
    SwitchStmtNode |
    ForEachStmtNode |
    ForLoopStmtNode |
    WhileLoopStmtNode |
    DoWhileLoopStmtNode |
    BreakStmtNode |
    ContinueStmtNode |
    ReturnStmtNode |
    EnumDeclStmtNode |
    TypeDeclStmtNode |
    ExternDeclStmtNode |
    VariableDeclStmtNode;

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
    SwitchCaseNode |
    ProgramNode |
    TypeNode |
    ExprNode |
    StmtNode;

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

const ahead = (env: Env, i: int, tag: TT, text: string | null = null): boolean => {
  if (env.i + i >= env.tokens.length) return false;
  const token = env.tokens[env.i + i]!;
  return token.tag === tag && (!text || token.text === text);
};

const check = (env: Env, tag: TT, text: string | null = null): boolean => {
  return ahead(env, 0, tag, text);
};

const consume = (env: Env, node: BaseNode | null,
                 tag: TT, text: string | null = null): boolean => {
  if (!check(env, tag, text)) return false;
  assert(env.i < env.tokens.length);
  const token = env.tokens[env.i++]!;
  if (node) includeToken(node, token);
  return true;
};

const expect = (env: Env, error: string, node: BaseNode | null,
                tag: TT, text: string | null = null): boolean => {
  if (consume(env, node, tag, text)) return true;
  env.diagnostics.push({pos: cursor(env), error});
  return false;
};

const includeToken = (node: BaseNode, token: Token): void => {
  node.pos = Math.min(node.pos, token.pos);
  node.end = Math.max(node.pos, token.end);
};

const makeEmptyBaseNode = (): BaseNode => {
  return {pos: 0, end: 0, text: '', children: []};
};

const makeBaseNode = (env: Env): BaseNode => {
  const i = env.i;
  const tokens = env.tokens;
  const result = makeEmptyBaseNode();
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
                      tag: TT, alt: string): string => {
  return expect(env, error, base, tag) ? env.tokens[env.i - 1]!.text : alt;
};

const parseIdentifier = (env: Env, alt: string): IdentifierNode => {
  const base = makeBaseNode(env);
  base.text = curTokenText(env, 'Expected: identifier', base, TT.Identifier, alt);
  return {base, tag: NT.Identifier};
};

const parseFieldName = (env: Env, alt: string): IdentifierNode => {
  let tag = TT.Identifier as TT;
  if (env.i < env.tokens.length) {
    const t = env.tokens[env.i]!.tag;
    const okay = t === TT.BoolLiteral || t === TT.NullLiteral ||
                 t === TT.VoidLiteral || t === TT.Keyword;
    if (okay) tag = t;
  }
  const base = makeBaseNode(env);
  base.text = curTokenText(env, 'Expected: field name', base, tag, alt);
  return {base, tag: NT.Identifier};
};

const parseKeyword = (env: Env): KeywordNode => {
  const base = makeBaseNode(env);
  base.text = curTokenText(env, 'Expected: keyword', base, TT.Keyword, '');
  return {base, tag: NT.Keyword};
};

const parseOperator = (env: Env): OperatorNode => {
  const base = makeBaseNode(env);
  base.text = curTokenText(env, 'Expected: operator', base, TT.Symbol, '');
  return {base, tag: NT.Operator};
};

const parseIdentifierType = (env: Env, alt: string): IdentifierTypeNode => {
  const base = makeBaseNode(env);
  base.text = curTokenText(env, 'Expected: type', base, TT.Identifier, alt);
  return {base, tag: NT.IdentifierType};
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
  return {base, tag: NT.ArgType, name, type, opt};
};

const parseFieldType = (env: Env, alt: string): FieldTypeNode => {
  const base = makeBaseNode(env);
  const name = parseFieldName(env, alt);
  append(base, name);
  expect(env, 'Expected: :', base, TT.Symbol, ':');
  const type = parseType(env);
  append(base, type);
  return {base, tag: NT.FieldType, name, type};
};

const parseClosureType = (env: Env): ClosureTypeNode | null => {
  const parseBody = (base: BaseNode, args: ArgTypeNode[]): ClosureTypeNode => {
    expect(env, 'Expected: =>', base, TT.Symbol, '=>');
    const result = parseType(env);
    append(base, result);
    return {base, tag: NT.ClosureType, args, result};
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
    return {base: makeTokenNode(env), tag: NT.IdentifierType};
  }

  if (!check(env, TT.Identifier)) {
    env.diagnostics.push({pos: cursor(env), error: 'Expected: type'});
    return {base: makeBaseNode(env), tag: NT.ErrorType};
  } else if (ahead(env, 1, TT.Symbol, '.')) {
    const base = makeBaseNode(env);
    const root = parseIdentifier(env, '$enum');
    append(base, root);
    expect(env, 'Expected: .', base, TT.Symbol, '.');
    const field = parseIdentifier(env, '$option');
    append(base, field);
    return {base, tag: NT.ValueType, root, field};
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
  return {base, tag: NT.GenericType, name, args};
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
      return {base, tag: NT.TupleType, elements};
    }
    do {
      elements.push(parseType(env));
      append(base, elements[elements.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, ']'));
    expect(env, 'Expected: ]', base, TT.Symbol, ']');
    return {base, tag: NT.TupleType, elements};
  }

  if (check(env, TT.Symbol, '{')) {
    const base = makeBaseNode(env);
    expect(env, 'Expected: {', base, TT.Symbol, '{');
    const fields = [] as FieldTypeNode[];
    if (consume(env, base, TT.Symbol, '}')) {
      return {base, tag: NT.StructType, fields};
    }
    do {
      fields.push(parseFieldType(env, 'Item'));
      append(base, fields[fields.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, '}'));
    expect(env, 'Expected: }', base, TT.Symbol, '}');
    return {base, tag: NT.StructType, fields};
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
    return {base, tag: NT.ArrayType, element: result};
  }
  return result;
};

const parseType = (env: Env): TypeNode => {
  const result = parseTermType(env);
  if (!check(env, TT.Symbol, '|')) return result;

  const base = makeBaseNode(env);
  const options = [result] as TypeNode[];
  append(base, result);
  expect(env, 'Expected: |', base, TT.Symbol, '|');
  do {
    options.push(parseTermType(env));
    append(base, options[options.length - 1]!);
  } while (consume(env, base, TT.Symbol, '|'));
  return {base, tag: NT.UnionType, options};
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
  return {base, tag: NT.ArgDefinition, name, type, def};
};

const parseCallArgs = (env: Env): CallArgsNode => {
  const base = makeBaseNode(env);
  const args = [] as ExprNode[];
  if (check(env, TT.Symbol, ')')) return {base, tag: NT.CallArgs, args};
  do {
    args.push(parseExpr(env));
    append(base, args[args.length - 1]!);
  } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, ')'));
  return {base, tag: NT.CallArgs, args};
};

const parseFieldExpr = (env: Env, alt: string): FieldExprNode => {
  const base = makeBaseNode(env);
  const name = parseFieldName(env, alt);
  append(base, name);
  const expr = ((): ExprNode => {
    if (consume(env, base, TT.Symbol, ':')) return parseExpr(env);
    const prev = name.base;
    const pos = prev.pos; const end = prev.end; const text = prev.text;
    const copy = {pos, end, text, children: []} as BaseNode;
    return {base: copy, tag: NT.VariableExpr};
  })();
  append(base, expr);
  return {base, tag: NT.FieldExpr, name, expr};
};

const parseFunctionBody = (env: Env): BlockStmtNode => {
  const block = parseBlockStmt(env);
  if (block) return block;

  const base = makeBaseNode(env);
  const stmt = ((): ReturnStmtNode => {
    const base = makeBaseNode(env);
    const expr = parseExpr(env);
    append(base, expr);
    return {base, tag: NT.ReturnStmt, expr};
  })();
  append(base, stmt);
  return {base, tag: NT.BlockStmt, stmts: [stmt]};
};

const parseClosureExpr = (env: Env): ClosureExprNode | null => {
  const parseBody = (base: BaseNode, args: ArgDefinitionNode[]): ClosureExprNode => {
    expect(env, 'Expected: :', base, TT.Symbol, ':');
    const result = parseType(env);
    append(base, result);
    expect(env, 'Expected: =>', base, TT.Symbol, '=>');
    const body = parseFunctionBody(env);
    append(base, body);
    return {base, tag: NT.ClosureExpr, args, result, body};
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
    const args = [parseArgDefinition(env, '$0')] as ArgDefinitionNode[];
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
  expect(env, 'Expected: )', base, TT.Symbol, ')');
  return {base, tag: NT.ConstructorCallExpr, cls, args};
};

const parseRootExpr = (env: Env): ExprNode => {
  const token = (): BaseNode => makeTokenNode(env);
  if (check(env, TT.Identifier))  return {base: token(), tag: NT.VariableExpr};
  if (check(env, TT.DblLiteral))  return {base: token(), tag: NT.DblLiteralExpr};
  if (check(env, TT.IntLiteral))  return {base: token(), tag: NT.IntLiteralExpr};
  if (check(env, TT.StrLiteral))  return {base: token(), tag: NT.StrLiteralExpr};
  if (check(env, TT.BoolLiteral)) return {base: token(), tag: NT.BoolLiteralExpr};
  if (check(env, TT.NullLiteral)) return {base: token(), tag: NT.NullLiteralExpr};

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
      return {base, tag: NT.ArrayExpr, elements};
    }
    do {
      elements.push(parseExpr(env));
      append(base, elements[elements.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, ']'));
    expect(env, 'Expected: ]', base, TT.Symbol, ']');
    return {base, tag: NT.ArrayExpr, elements};
  }

  if (check(env, TT.Symbol, '{')) {
    const base = makeBaseNode(env);
    expect(env, 'Expected: {', base, TT.Symbol, '{');
    const fields = [] as FieldExprNode[];
    if (consume(env, base, TT.Symbol, '}')) {
      return {base, tag: NT.StructExpr, fields};
    }
    do {
      fields.push(parseFieldExpr(env, '$field'));
      append(base, fields[fields.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, '}'));
    expect(env, 'Expected: }', base, TT.Symbol, '}');
    return {base, tag: NT.StructExpr, fields};
  }

  if (check(env, TT.TemplateStart)) {
    const base = makeBaseNode(env);
    const prefix = {base: token(), tag: NT.Template} as TemplateNode;
    const suffixes = [] as [ExprNode, TemplateNode][];
    append(base, prefix);
    while (true) {
      const before = env.i;
      const expr = parseExpr(env);
      append(base, expr);
      if (check(env, TT.TemplateEnd)) {
        const template = {base: token(), tag: NT.Template} as TemplateNode;
        suffixes.push([expr, template]);
        append(base, template);
        break;
      }
      if (expect(env, 'Expected: template', base, TT.TemplateMid)) env.i--;
      const template = {base: token(), tag: NT.Template} as TemplateNode;
      suffixes.push([expr, template]);
      append(base, template);
      if (env.i === before && !advance(env)) break;
    }
    return {base, tag: NT.TemplateExpr, prefix, suffixes};
  }

  env.diagnostics.push({pos: cursor(env), error: 'Expected: expression'});
  return {base: makeBaseNode(env), tag: NT.ErrorExpr};
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
      expect(env, 'Expected: )', base, TT.Symbol, ')');
      expr = {base, tag: NT.FunctionCallExpr, fn: expr, args};
      continue;
    }

    if (symbol === '.') {
      const base = makeBaseNode(env);
      append(base, expr);
      expect(env, 'Expected: .', base, TT.Symbol, '.');
      const field = parseFieldName(env, '$field');
      append(base, field);
      expr = {base, tag: NT.FieldAccessExpr, root: expr, field};
      continue;
    }

    if (symbol === '[') {
      const base = makeBaseNode(env);
      append(base, expr);
      expect(env, 'Expected: [', base, TT.Symbol, '[');
      const index = parseExpr(env);
      append(base, index);
      expect(env, 'Expected: ]', base, TT.Symbol, ']');
      expr = {base, tag: NT.IndexAccessExpr, root: expr, index};
      continue;
    }

    if (symbol === '!') {
      const base = makeBaseNode(env);
      append(base, expr);
      const op = parseOperator(env);
      append(base, op);
      expr = {base, tag: NT.UnarySuffixOpExpr, op, expr};
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
    expr = {base, tag: NT.UnarySuffixOpExpr, op, expr};
  }

  while (pre.length > 0) {
    const base = makeBaseNode(env);
    const op = pre.pop()!;
    append(base, op);
    append(base, expr);
    expr = {base, tag: NT.UnaryPrefixOpExpr, op, expr};
  }
  return expr;
};

const parseCastExpr = (env: Env, expr: ExprNode): ExprNode => {
  if (!check(env, TT.Keyword, 'as')) return expr;

  const base = makeBaseNode(env);
  append(base, expr);
  expect(env, 'Expected: as', base, TT.Keyword, 'as');
  const type = parseType(env);
  append(base, type);
  return {base, tag: NT.CastExpr, expr, type};
};

const parseBinaryOpExpr = (env: Env): ExprNode => {
  const terms = [] as ExprNode[];
  const ops = [] as [OperatorNode, Precedence][];
  terms.push(parseCastExpr(env, parseUnaryOpExpr(env)));

  const evalOneOp = (): void => {
    const rhs = terms.pop()!;
    const lhs = terms.pop()!;
    const op = ops.pop()![0];
    const base = makeBaseNode(env);
    append(base, lhs);
    append(base, op);
    append(base, rhs);
    terms.push({base, tag: NT.BinaryOpExpr, op, lhs, rhs});
  };

  while (true) {
    if (!check(env, TT.Symbol)) break;
    const pos = cursor(env);
    const symbol = env.tokens[env.i]!.text;
    const precedence = binops.get(symbol) ?? null;
    if (!precedence) break;

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
  if (ct) return parseCastExpr(env, ct);

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
    return {base, tag: NT.TernaryExpr, cond: expr, lhs, rhs};
  }

  if (assignment.has(symbol)) {
    const base = makeBaseNode(env);
    append(base, expr);
    const op = parseOperator(env);
    append(base, op);
    const rhs = parseExpr(env);
    append(base, rhs);
    return {base, tag: NT.AssignmentExpr, op, lhs: expr, rhs};
  }
  return expr;
};

// Statement grammar

const parseExprStmt = (env: Env): ExprStmtNode => {
  const base = makeBaseNode(env);
  const expr = parseExpr(env);
  append(base, expr);
  return {base, tag: NT.ExprStmt, expr};
};

const parseTrivialStmt = (env: Env, error: string, text: string,
                          parent: BaseNode | null): StmtNode => {
  const base = makeBaseNode(env);
  if (consume(env, base, TT.Symbol, text)) {
    return {base, tag: NT.EmptyStmt};
  }
  const expr = parseExpr(env);
  append(base, expr);
  expect(env, error, parent ?? base, TT.Symbol, text);
  return {base, tag: NT.ExprStmt, expr};
};

const parseTrivialStmtAndSemiColon =
    (env: Env, parent: BaseNode | null = null): StmtNode => {
  return parseTrivialStmt(env, 'Expected: ;', ';', parent);
};

const parseTrivialStmtAndParen =
    (env: Env, parent: BaseNode | null = null): StmtNode => {
  return parseTrivialStmt(env, 'Expected: )', ')', parent);
};

const parseBlockStmt = (env: Env): BlockStmtNode | null => {
  if (!check(env, TT.Symbol, '{')) return null;

  const base = makeBaseNode(env);
  expect(env, 'Expected: {', base, TT.Symbol, '{');
  const stmts = [] as StmtNode[];
  while (!consume(env, base, TT.Symbol, '}')) {
    const before = env.i;
    stmts.push(parseDecl(env));
    append(base, stmts[stmts.length - 1]!);
    if (env.i === before && !advance(env)) break;
  }
  return {base, tag: NT.BlockStmt, stmts};
};

const parseSwitchCase = (env: Env): SwitchCaseNode | null => {
  const last = check(env, TT.Keyword, 'default');
  if (!last && !check(env, TT.Keyword, 'case')) return null;

  const base = makeBaseNode(env);
  let expr = null as ExprNode | null;
  if (last) {
    expect(env, 'Expected: default', base, TT.Keyword, 'default');
  } else {
    expect(env, 'Expected: case', base, TT.Keyword, 'case');
    expr = parseExpr(env);
    append(base, expr);
  }
  expect(env, 'Expected: :', base, TT.Symbol, ':');
  const then = ((): StmtNode => {
    if (!check(env, TT.Keyword, 'case')) return parseStmt(env);
    return {base: makeEmptyBaseNode(), tag: NT.EmptyStmt};
  })();
  append(base, then);
  return {base, tag: NT.SwitchCase, expr, then};
};

const parseSwitchStmt = (env: Env): SwitchStmtNode => {
  const base = makeBaseNode(env);
  expect(env, 'Expected: switch', base, TT.Keyword, 'switch');
  expect(env, 'Expected: (', base, TT.Symbol, '(');
  const expr = parseExpr(env);
  append(base, expr);
  expect(env, 'Expected: )', base, TT.Symbol, ')');
  expect(env, 'Expected: {', base, TT.Symbol, '{');
  const cases = [] as SwitchCaseNode[];
  let result = null as SwitchCaseNode | null;
  while (result = parseSwitchCase(env)) {
    cases.push(result);
    append(base, result);
  }
  expect(env, 'Expected: }', base, TT.Symbol, '}');
  return {base, tag: NT.SwitchStmt, expr, cases};
};

const parseStmt = (env: Env): StmtNode => {
  const parseIfStmt = (): StmtNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: if', base, TT.Keyword, 'if');
    expect(env, 'Expected: (', base, TT.Symbol, '(');
    const cond = parseExpr(env);
    append(base, cond);
    expect(env, 'Expected: )', base, TT.Symbol, ')');
    const then = parseStmt(env);
    append(base, then);

    let elseCase = null as StmtNode | null;
    if (consume(env, base, TT.Keyword, 'else')) {
      elseCase = parseStmt(env);
      append(base, elseCase);
    }
    return {base, tag: NT.IfStmt, cond, then, else: elseCase};
  };

  const parseForLoop = (): StmtNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: for', base, TT.Keyword, 'for');
    expect(env, 'Expected: (', base, TT.Symbol, '(');
    let init = null as StmtNode | null;

    if (check(env, TT.Keyword, 'const') || check(env, TT.Keyword, 'let')) {
      const keyword = parseKeyword(env);
      append(base, keyword);
      const name = parseIdentifier(env, '$iter');
      append(base, name);

      if (consume(env, base, TT.Keyword, 'of')) {
        const expr = parseExpr(env);
        append(base, expr);
        expect(env, 'Expected: )', base, TT.Symbol, ')');
        const body = parseStmt(env);
        append(base, body);
        return {base, tag: NT.ForEachStmt, keyword, name, expr, body};
      }

      const parent = base;
      init = ((): StmtNode => {
        const base = makeBaseNode(env);
        append(base, keyword);
        append(base, name);
        expect(env, 'Expected: =', base, TT.Symbol, '=');
        const expr = parseExpr(env);
        append(base, expr);
        expect(env, 'Expected: ;', parent, TT.Symbol, ';');
        return {base, tag: NT.VariableDeclStmt, keyword, name, expr};
      })();
    } else {
      init = parseTrivialStmtAndSemiColon(env, base);
    }

    append(base, init!);
    const cond = parseExprStmt(env);
    append(base, cond);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    const post = parseTrivialStmtAndParen(env, base);
    append(base, post);
    const body = parseStmt(env);
    append(base, body);
    return {base, tag: NT.ForLoopStmt, init: init!, cond, post, body};
  };

  const parseDoWhileLoop = (): StmtNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: do', base, TT.Keyword, 'do');
    const body = parseStmt(env);
    append(base, body);
    expect(env, 'Expected: while', base, TT.Keyword, 'while');
    expect(env, 'Expected: (', base, TT.Symbol, '(');
    const cond = parseExpr(env);
    append(base, cond);
    expect(env, 'Expected: )', base, TT.Symbol, ')');
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, tag: NT.DoWhileLoopStmt, cond, body};
  };

  const parseWhileLoop = (): StmtNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: while', base, TT.Keyword, 'while');
    expect(env, 'Expected: (', base, TT.Symbol, '(');
    const cond = parseExpr(env);
    append(base, cond);
    expect(env, 'Expected: )', base, TT.Symbol, ')');
    const body = parseStmt(env);
    append(base, body);
    return {base, tag: NT.WhileLoopStmt, cond, body};
  };

  const parseThrow = (): StmtNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: throw', base, TT.Keyword, 'throw');
    const expr = parseExpr(env);
    append(base, expr);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, tag: NT.ThrowStmt, expr};
  };

  const parseReturn = (): StmtNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: return', base, TT.Keyword, 'return');
    if (consume(env, base, TT.Symbol, ';')) {
      return {base, tag: NT.ReturnStmt, expr: null};
    }
    const expr = parseExpr(env);
    append(base, expr);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, tag: NT.ReturnStmt, expr};
  };

  const parseBreak = (): StmtNode => {
    const base = makeTokenNode(env);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, tag: NT.BreakStmt};
  };

  const parseContinue = (): StmtNode => {
    const base = makeTokenNode(env);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, tag: NT.BreakStmt};
  };

  if (check(env, TT.Keyword)) {
    const keyword = env.tokens[env.i]!.text;
    if (keyword === 'if')        return parseIfStmt();
    if (keyword === 'for')       return parseForLoop();
    if (keyword === 'while')     return parseWhileLoop();
    if (keyword === 'do')        return parseDoWhileLoop();
    if (keyword === 'throw')     return parseThrow();
    if (keyword === 'return')    return parseReturn();
    if (keyword === 'break')     return parseBreak();
    if (keyword === 'continue')  return parseContinue();
    if (keyword === 'switch')    return parseSwitchStmt(env);
  }

  const block = parseBlockStmt(env);
  return block ?? parseTrivialStmtAndSemiColon(env);
};

const parseDecl = (env: Env): StmtNode => {
  const parseVariableDecl = (): StmtNode => {
    const base = makeBaseNode(env);
    const keyword = parseKeyword(env);
    append(base, keyword);
    const name = parseIdentifier(env, '$lhs');
    append(base, name);
    expect(env, 'Expected: =', base, TT.Symbol, '=');
    const expr = parseExpr(env);
    append(base, expr);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, tag: NT.VariableDeclStmt, keyword, name, expr};
  };

  const parseEnumDecl = (): StmtNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: enum', base, TT.Keyword, 'enum');
    const name = parseIdentifier(env, '$enum');
    append(base, name);
    expect(env, 'Expected: {', base, TT.Symbol, '{');
    const values = [] as IdentifierNode[];
    if (consume(env, base, TT.Symbol, '}')) {
      return {base, tag: NT.EnumDeclStmt, name, values};
    }
    do {
      values.push(parseIdentifier(env, `$${values.length}`));
      append(base, values[values.length - 1]!);
    } while (consume(env, base, TT.Symbol, ',') && !check(env, TT.Symbol, '}'));
    expect(env, 'Expected: }', base, TT.Symbol, '}');
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, tag: NT.EnumDeclStmt, name, values};
  };

  const parseTypeDecl = (): StmtNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: type', base, TT.Identifier, 'type');
    const name = parseIdentifier(env, '$type');
    append(base, name);
    expect(env, 'Expected: =', base, TT.Symbol, '=');
    const type = parseType(env);
    append(base, type);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, tag: NT.TypeDeclStmt, name, type};
  };

  const parseExternDecl = (): StmtNode => {
    const base = makeBaseNode(env);
    expect(env, 'Expected: declare', base, TT.Keyword, 'declare');
    expect(env, 'Expected: const', base, TT.Keyword, 'const');
    const name = parseIdentifier(env, '$type');
    append(base, name);
    expect(env, 'Expected: :', base, TT.Symbol, ':');
    const type = parseType(env);
    append(base, type);
    expect(env, 'Expected: ;', base, TT.Symbol, ';');
    return {base, tag: NT.ExternDeclStmt, name, type};
  };

  if (check(env, TT.Keyword) ||
      (check(env, TT.Identifier, 'type') && ahead(env, 1, TT.Identifier))) {
    const keyword = env.tokens[env.i]!.text;
    if (keyword === 'const')     return parseVariableDecl();
    if (keyword === 'let')       return parseVariableDecl();
    if (keyword === 'enum')      return parseEnumDecl();
    if (keyword === 'type')      return parseTypeDecl();
    if (keyword === 'declare')   return parseExternDecl();
  }

  return parseStmt(env);
};

const parseProgram = (env: Env): ProgramNode => {
  const base = makeBaseNode(env);
  const stmts = [] as StmtNode[];
  while (env.i < env.tokens.length) {
    const before = env.i;
    stmts.push(parseDecl(env));
    append(base, stmts[stmts.length - 1]!);
    if (env.i === before) advance(env);
  }
  return {base, tag: NT.Program, stmts};
};

//////////////////////////////////////////////////////////////////////////////

// Parsing entry points

const getNodeText = (input: string, node: Node): string => {
  const base = node.base;
  return input.substring(base.pos, base.end);
};

const formatNode = (input: string, node: Node): string => {
  const base = node.base;
  const kind = NT[node.tag]!;
  const substr = getNodeText(input, node);
  const suffix = base.text.length > 0 ? `: ${base.text}`
                                      : substr.indexOf('\n') === -1
                                            ? `: ${substr}` : '';
  return `${base.pos}:${base.end}:${kind}${suffix}`;
};

const formatAST = (input: string, root: Node): string => {
  const lines = [] as string[];
  const recurse = (node: Node, depth: int): void => {
    const spacer = ' '.repeat(2 * depth);
    lines.push(`${spacer}${formatNode(input, node)}`);
    for (const child of node.base.children) recurse(child, depth + 1);
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

const parse = (input: string, diagnostics: Diagnostic[]): ProgramNode => {
  const tokens = lex(input, diagnostics);
  return parseProgram({input, tokens, diagnostics, i: 0});
};

//////////////////////////////////////////////////////////////////////////////

// Type system

enum TC {
  Dbl,
  Exn,
  Str,
  Bool,
  Null,
  Void,
  Error,
  Never,
  Array,
  Tuple,
  Union,
  Value,
  Struct,
  Closure,
  Nullable,
  Enum,
  Map,
  Set,
};

type DblType      = {tag: TC.Dbl};
type ExnType      = {tag: TC.Exn};
type StrType      = {tag: TC.Str};
type BoolType     = {tag: TC.Bool};
type NullType     = {tag: TC.Null};
type VoidType     = {tag: TC.Void};
type ErrorType    = {tag: TC.Error};
type NeverType    = {tag: TC.Never};
type ArrayType    = {tag: TC.Array, element: Type};
type TupleType    = {tag: TC.Tuple, elements: Type[]};
type UnionType    = {tag: TC.Union, name: string | null, options: StructType[]};
type ValueType    = {tag: TC.Value, root: EnumType, field: string};
type StructType   = {tag: TC.Struct, name: string, fields: Map<string, Type>};
type ClosureType  = {tag: TC.Closure, args: Map<string, ArgType>, result: Type};
type NullableType = {tag: TC.Nullable, base: Type};
type EnumType     = {tag: TC.Enum, name: string, values: Set<string>};
type MapType      = {tag: TC.Map, key: Type, val: Type};
type SetType      = {tag: TC.Set, element: Type};

type ArgType = {type: Type, opt: boolean};

type Type =
    DblType |
    ExnType |
    StrType |
    BoolType |
    NullType |
    VoidType |
    ErrorType |
    NeverType |
    ArrayType |
    TupleType |
    UnionType |
    ValueType |
    StructType |
    ClosureType |
    NullableType |
    EnumType |
    MapType |
    SetType;

const typeMatches = (a: Type, b: Type): boolean => {
  if (a.tag === TC.Error || b.tag === TC.Error) return true;
  if (a.tag !== b.tag) return false;
  if (a === b) return true;

  switch (a.tag) {
    // Trivial given a tag match:
    case TC.Dbl: return true;
    case TC.Exn: return true;
    case TC.Str: return true;
    case TC.Bool: return true;
    case TC.Null: return true;
    case TC.Void: return true;
    case TC.Never: return true;

    // Structs and enums are nominal:
    case TC.Enum: return false;
    case TC.Struct: return false;

    case TC.Array: {
      if (b.tag !== TC.Array) throw new Error();
      return typeMatches(a.element, b.element);
    }
    case TC.Tuple: {
      if (b.tag !== TC.Tuple) throw new Error();
      if (a.elements.length !== b.elements.length) return false;
      for (let i = 0; i < a.elements.length; i++) {
        if (!typeMatches(a.elements[i]!, b.elements[i]!)) return false;
      }
      return true;
    }
    case TC.Union: {
      if (b.tag !== TC.Union) throw new Error();
      if (a.options.length !== b.options.length) return false;
      const a_options = Array.from(a.options).sort();
      const b_options = Array.from(b.options).sort();
      for (let i = 0; i < a_options.length; i++) {
        if (a_options[i]! !== b_options[i]!) return false;
      }
      return true;
    }
    case TC.Value: {
      if (b.tag !== TC.Value) throw new Error();
      return a.root === b.root && a.field === b.field;
    }
    case TC.Closure: {
      if (b.tag !== TC.Closure) throw new Error();
      const aargs = Array.from(a.args.values());
      const bargs = Array.from(b.args.values());
      if (aargs.length !== bargs.length) return false;
      for (let i = 0; i < aargs.length; i++) {
        if (aargs[i]!.opt !== bargs[i]!.opt) return false;
        if (!typeMatches(aargs[i]!.type, bargs[i]!.type)) return false;
      }
      return typeMatches(a.result, b.result);
    }
    case TC.Nullable: {
      if (b.tag !== TC.Nullable) throw new Error();
      return typeMatches(a.base, b.base);
    }
    case TC.Map: {
      if (b.tag !== TC.Map) throw new Error();
      return typeMatches(a.key, b.key) && typeMatches(a.val, b.val);
    }
    case TC.Set: {
      if (b.tag !== TC.Set) throw new Error();
      return typeMatches(a.element, b.element);
    }
  }
};

const typeAccepts = (annot: Type, value: Type): boolean => {
  if (annot === value) return true;
  if (value.tag === TC.Never) return true;
  if (annot.tag === TC.Union) {
    const annots = annot.options;
    const values = value.tag === TC.Union ? value.options : [value];
    return values.every((v: Type): boolean =>
        annots.some((a: Type): boolean => typeAccepts(a, v)));
  }
  if (annot.tag === TC.Nullable) {
    if (value.tag === TC.Null) return true;
    const left = value.tag === TC.Nullable ? value.base : value;
    return typeAccepts(annot.base, left);
  }
  if (annot.tag === TC.Enum) {
    if (value.tag === TC.Value && annot === value.root) return true;
  }
  return typeMatches(annot, value);
};

const typesCompareEqual = (lhs: Type, rhs: Type): boolean => {
  return typeAccepts(lhs, rhs) || typeAccepts(rhs, lhs);
};

const typeDesc = (type: Type): string => {
  if (type.tag === TC.Enum || type.tag === TC.Struct) return type.name;
  if (type.tag === TC.Union) return type.name ?? typeDescFull(type);
  return typeDescFull(type);
};

const typeDescFull = (type: Type): string => {
  switch (type.tag) {
    case TC.Dbl: return 'number';
    case TC.Exn: return 'Error';
    case TC.Str: return 'string';
    case TC.Bool: return 'boolean';
    case TC.Null: return 'null';
    case TC.Void: return 'void';
    case TC.Error: return '<error>';
    case TC.Never: return '<never>';
    case TC.Array: {
      const inner = typeDesc(type.element);
      const paren = type.element.tag === TC.Union && !type.element.name;
      return paren ? `(${inner})[]` : `${inner}[]`;
    }
    case TC.Tuple: return `[${type.elements.map(typeDesc).join(', ')}]`;
    case TC.Union: return type.options.map(typeDesc).join(' | ');
    case TC.Value: return `${type.root.name}.${type.field}`;
    case TC.Struct: {
      const parts = [] as string[];
      for (const item of type.fields) {
        parts.push(`${item[0]}: ${typeDesc(item[1])}`);
      }
      return `{${parts.join(', ')}}`;
    }
    case TC.Closure: {
      const parts = [] as string[];
      for (const item of type.args) {
        const opt = item[1].opt ? '?' : '';
        parts.push(`${item[0]}${opt}: ${typeDesc(item[1].type)}`);
      }
      return `(${parts.join(', ')}) => ${typeDesc(type.result)}`;
    }
    case TC.Nullable: {
      const inner = typeDesc(type.base);
      const paren = type.base.tag === TC.Union && !type.base.name;
      return paren ? `(${inner})?` : `${inner}?`;
    }
    case TC.Enum: return `enum {${Array.from(type.values).join(', ')}}`;
    case TC.Map:  return `Map<${typeDesc(type.key)}, ${typeDesc(type.val)}>`;
    case TC.Set:  return `Set<${typeDesc(type.element)}>`;
  }
};

const typeWithoutNull = (type: Type, registry: TypeRegistry): Type => {
  if (type.tag === TC.Null) return registry.never;
  if (type.tag === TC.Nullable) return type.base;
  return type;
};

type UnionErrorCallback = (error: string) => void;

const typeUnionHelper = (a: Type, b: Type, registry: TypeRegistry,
                         error: UnionErrorCallback): Type => {
  if (a.tag === TC.Error) return a;
  if (b.tag === TC.Error) return b;
  if (typeAccepts(a, b)) return a;
  if (typeAccepts(b, a)) return b;

  // A nontrivial union is a union (other than error, the top/bottom type)
  // where neither a <= b nor b <= a. Ignoring null, there are a few cases:

  // Nontrivial union: enum literals of the same enum type union to that enum.
  if (a.tag === TC.Value && b.tag === TC.Value) {
    if (a.root === b.root) return a.root;
  }

  // Nontrivial union: tagged struct types with distinct tags can be unioned.
  // In this case, the tags must be enum values of the same enum type.
  if ((a.tag === TC.Struct || a.tag === TC.Union) &&
      (b.tag === TC.Struct || b.tag === TC.Union)) {
    const xs = a.tag === TC.Struct ? [a] : a.options;
    const ys = b.tag === TC.Struct ? [b] : b.options;
    const seen = new Map() as Map<string, StructType>;
    const options = [] as StructType[];
    let root = null as EnumType | null;

    const addOption = (x: StructType): boolean => {
      let success = false;
      const tag = x.fields.get('tag');
      const prev = tag && tag.tag === TC.Value ? seen.get(tag.field) : null;
      if (!tag) {
        error(`Can't union ${x.name}. It must have a 'tag' field.`);
      } else if (tag.tag !== TC.Value) {
        error(`Can't union ${x.name}. 'tag' must be an enum value; got: ${typeDesc(tag)}.`);
      } else if (root && root !== tag.root) {
        const name = tag.root.name;
        const y = Array.from(seen.values())[0]!;
        error(`Can't union ${y.name} and ${x.name}. Tags mismatch: ${root.name} vs. ${name}`);
      } else if (prev && prev !== x) {
        error(`Can't union ${prev.name} and ${x.name}. Tags collide: ${typeDesc(tag)}`);
      } else {
        options.push(x);
        root = tag.root;
        success = true;
      }
      return success;
    };

    for (const x of xs) if (!addOption(x)) return registry.error;
    for (const y of ys) if (!addOption(y)) return registry.error;
    assert(options.length > 1);
    return {tag: TC.Union, name: null, options};
  }

  error(`Unsupported union: ${typeDesc(a)} | ${typeDesc(b)}`);
  return registry.error;
};

const typeUnion = (a: Type, b: Type, registry: TypeRegistry,
                   error: UnionErrorCallback): Type => {
  const an = typeWithoutNull(a, registry);
  const bn = typeWithoutNull(b, registry);
  const rn = typeUnionHelper(an, bn, registry, error);

  // If neither a nor b was nullable, return the helper's result.
  if (an === a && bn === b) return rn;

  // Avoid constructing error?, never?, or null?.
  if (rn.tag === TC.Error || rn.tag === TC.Null) return rn;
  if (rn.tag === TC.Never) return registry.null;

  // As an optimization, avoid constructing a new nullable type if our
  // result is equivalent to either of our inputs.
  if (rn === an && an !== a) return a;
  if (rn === bn && bn !== b) return b;
  return {tag: TC.Nullable, base: rn};
};

//////////////////////////////////////////////////////////////////////////////

type TypeRegistry = {
  dbl: DblType,
  exn: ExnType,
  str: StrType,
  bool: BoolType,
  null: NullType,
  void: VoidType,
  error: ErrorType,
  never: NeverType,

  primitives: Map<string, Type>,
};

const makeTypeRegistry = (): TypeRegistry => {
  const dt = {tag: TC.Dbl} as DblType;
  const xt = {tag: TC.Exn} as ExnType;
  const st = {tag: TC.Str} as StrType;
  const bt = {tag: TC.Bool} as BoolType;
  const nt = {tag: TC.Null} as NullType;
  const vt = {tag: TC.Void} as VoidType;
  const et = {tag: TC.Error} as ErrorType;
  const ot = {tag: TC.Never} as NeverType;

  const primitives = new Map() as Map<string, Type>;
  for (const type of [dt, st, bt, nt, vt, et, ot]) {
    primitives.set(typeDesc(type), type);
  }

  return {
    dbl: dt,
    exn: xt,
    str: st,
    bool: bt,
    null: nt,
    void: vt,
    error: et,
    never: ot,
    primitives,
  };
};

//////////////////////////////////////////////////////////////////////////////

// Bindings

type Bindable = VariableExprNode;

type Bindings = {
  indices: Map<Bindable, int>,
  mutated: boolean[],
};

type BindingScope = Map<string, int>;

type BindingEnv = {
  nextIndex: int,
  result: Bindings,
  scopes: BindingScope[],
};

const makeBindingScope = (): BindingScope => { return new Map(); };

const bindName = (env: BindingEnv, name: string, force: boolean = false): void => {
  const scope = env.scopes[0]!;
  if (!force && scope.has(name)) return;
  scope.set(name, env.nextIndex++);
  env.result.mutated.push(false);
};

const resolveName = (env: BindingEnv, name: string): int | null => {
  for (const scope of env.scopes) {
    const result = scope.get(name) ?? null;
    if (result !== null) return result;
  }
  return null;
};

const resolveExpr = (env: BindingEnv, expr: ExprNode): boolean => {
  switch (expr.tag) {
    // Trivial cases:
    case NT.ErrorExpr: return true;
    case NT.DblLiteralExpr: return true;
    case NT.IntLiteralExpr: return true;
    case NT.StrLiteralExpr: return true;
    case NT.BoolLiteralExpr: return true;
    case NT.NullLiteralExpr: return true;

    case NT.VariableExpr: {
      const index = resolveName(env, expr.base.text);
      if (index !== null) env.result.indices.set(expr, index);
      return true;
    }
    case NT.AssignmentExpr: {
      resolveExpr(env, expr.lhs);
      resolveExpr(env, expr.rhs);

      const lhs = expr.lhs;
      if (lhs.tag === NT.VariableExpr) {
        const index = resolveName(env, lhs.base.text);
        if (index !== null) env.result.mutated[index] = true;
      }
      return true;
    }
    case NT.BinaryOpExpr: {
      resolveExpr(env, expr.lhs);
      resolveExpr(env, expr.rhs);
      return true;
    }
    case NT.UnaryPrefixOpExpr: {
      resolveExpr(env, expr.expr);
      return true;
    }
    case NT.UnarySuffixOpExpr: {
      resolveExpr(env, expr.expr);
      return true;
    }
    case NT.CastExpr: {
      resolveExpr(env, expr.expr);
      return true;
    }
    case NT.ArrayExpr: {
      for (const value of expr.elements) resolveExpr(env, value);
      return true;
    }
    case NT.StructExpr: {
      for (const field of expr.fields) resolveExpr(env, field.expr);
      return true;
    }
    case NT.ClosureExpr: {
      env.scopes.unshift(makeBindingScope());
      for (const arg of expr.args) bindName(env, arg.name.base.text);
      const scope = env.scopes.shift()!;
      resolveBlock(env, expr.body.stmts, scope);
      return true;
    }
    case NT.FunctionCallExpr: {
      resolveExpr(env, expr.fn);
      for (const arg of expr.args.args) resolveExpr(env, arg);
      return true;
    }
    case NT.ConstructorCallExpr: {
      for (const arg of expr.args.args) resolveExpr(env, arg);
      return true;
    }
    case NT.FieldAccessExpr: {
      resolveExpr(env, expr.root);
      return true;
    }
    case NT.IndexAccessExpr: {
      resolveExpr(env, expr.root);
      resolveExpr(env, expr.index);
      return true;
    }
    case NT.TernaryExpr: {
      resolveExpr(env, expr.cond);
      resolveExpr(env, expr.lhs);
      resolveExpr(env, expr.rhs);
      return true;
    }
    case NT.TemplateExpr: {
      for (const part of expr.suffixes) resolveExpr(env, part[0]);
      return true;
    }
  }
};

const resolveStmt = (env: BindingEnv, stmt: StmtNode): boolean => {
  switch (stmt.tag) {
    // Trivial cases:
    case NT.EmptyStmt: return true;
    case NT.BreakStmt: return true;
    case NT.ContinueStmt: return true;
    case NT.EnumDeclStmt: return true;
    case NT.TypeDeclStmt: return true;
    case NT.ExternDeclStmt: return true;

    case NT.ExprStmt: return resolveExpr(env, stmt.expr);
    case NT.ThrowStmt: return resolveExpr(env, stmt.expr);
    case NT.VariableDeclStmt: return resolveExpr(env, stmt.expr);

    case NT.BlockStmt: {
      resolveBlock(env, stmt.stmts, makeBindingScope());
      return true;
    }
    case NT.IfStmt: {
      const elseCase = stmt.else;
      resolveExpr(env, stmt.cond);
      resolveStmt(env, stmt.then);
      if (elseCase) resolveStmt(env, elseCase);
      return true;
    }
    case NT.ReturnStmt: {
      const expr = stmt.expr;
      if (expr) resolveExpr(env, expr);
      return true;
    }
    case NT.SwitchStmt: {
      resolveExpr(env, stmt.expr);
      for (const c of stmt.cases) {
        const expr = c.expr;
        if (expr) resolveExpr(env, expr);
        resolveStmt(env, c.then);
      }
      return true;
    }
    case NT.ForEachStmt: {
      resolveExpr(env, stmt.expr);
      env.scopes.unshift(makeBindingScope());
      bindName(env, stmt.name.base.text);
      const scope = env.scopes.shift()!;
      resolveBlock(env, [stmt.body], scope);
      return true;
    }
    case NT.ForLoopStmt: {
      const block = [stmt.init, stmt.cond, stmt.body, stmt.post] as StmtNode[];
      resolveBlock(env, block, makeBindingScope());
      return true;
    }
    case NT.WhileLoopStmt: {
      resolveExpr(env, stmt.cond);
      resolveStmt(env, stmt.body);
      return true;
    }
    case NT.DoWhileLoopStmt: {
      resolveExpr(env, stmt.cond);
      resolveStmt(env, stmt.body);
      return true;
    }
  }
};

const resolveBlock = (env: BindingEnv, block: StmtNode[], scope: BindingScope): void => {
  env.scopes.unshift(scope);
  for (const stmt of block) {
    if (stmt.tag !== NT.EnumDeclStmt) continue;
    bindName(env, stmt.name.base.text, /*force=*/true);
  }
  for (const stmt of block) {
    if (stmt.tag !== NT.VariableDeclStmt) continue;
    bindName(env, stmt.name.base.text);
  }
  for (const stmt of block) resolveStmt(env, stmt);
  env.scopes.shift();
};

const resolveBindings = (input: string, program: ProgramNode): Bindings => {
  const result = {indices: new Map(), mutated: []} as Bindings;
  const env = {nextIndex: 0, result, scopes: []} as BindingEnv;
  resolveBlock(env, program.stmts, makeBindingScope());
  assert(env.scopes.length === 0);
  return result;
};

//////////////////////////////////////////////////////////////////////////////

// Control flow

enum CT { If, Switch };

type IfCond     = {tag: CT.If, expr: ExprNode, value: boolean};
type SwitchCond = {tag: CT.Switch, expr: ExprNode, value: ExprNode};

type FlowCond = IfCond | SwitchCond;

type FlowEdge = {
  pred: FlowNode,
  succ: FlowNode,
  cond: FlowCond | null,
};

type FlowNode = {
  label: string | null,
  preds: FlowEdge[],
  succs: FlowEdge[],
  stmt: StmtNode | null,
};

type FlowGraph = {
  entry: FlowNode,
  exit: FlowNode,
  nodes: Map<StmtNode, FlowNode>,
};

type CFG = (fn: ClosureExprNode, label: string | null, input: string) => FlowGraph;

const makeFlowGraph = ((): CFG => {

enum ST { Loop, Switch };

type LoopScope   = {tag: ST.Loop, break: FlowNode, continue: FlowNode};
type SwitchScope = {tag: ST.Switch, break: FlowNode};

type Scope = LoopScope | SwitchScope;

type FlowState = {
  exit: FlowNode,
  last: [FlowNode, FlowCond | null] | null,
  graph: FlowGraph,
  stack: Scope[],
};

const addFlowEdge = (pred: FlowNode, succ: FlowNode, cond: FlowCond | null = null): void => {
  const edge = {pred, succ, cond} as FlowEdge;
  pred.succs.push(edge);
  succ.preds.push(edge);
};

const addImplicitEdge = (state: FlowState, succ: FlowNode): void => {
  const last = state.last;
  if (last) addFlowEdge(last[0], succ, last[1]);
};

const makeRawFlowNode = (label: string | null, stmt: StmtNode | null = null): FlowNode => {
  return {label, preds: [], succs: [], stmt};
};

const pushFlowNode = (state: FlowState, stmt: StmtNode): FlowNode => {
  const node = makeRawFlowNode(null, stmt);
  state.graph.nodes.set(stmt, node);
  addImplicitEdge(state, node);
  return node;
};

const handleStmt = (state: FlowState, stmt: StmtNode): boolean => {
  switch (stmt.tag) {
    case NT.EnumDeclStmt:
    case NT.TypeDeclStmt:
    case NT.ExternDeclStmt:
    case NT.EmptyStmt:
      return true;

    case NT.BlockStmt: {
      for (const s of stmt.stmts) handleStmt(state, s);
      return true;
    }

    case NT.ExprStmt:
    case NT.VariableDeclStmt: {
      const node = pushFlowNode(state, stmt);
      state.last = [node, null];
      return true;
    }

    case NT.BreakStmt: {
      let scope = null as LoopScope | SwitchScope | null;
      for (let i = 0; i < state.stack.length; i++) {
        const frame = state.stack[state.stack.length - i - 1]!;
        if (frame.tag !== ST.Loop && frame.tag !== ST.Switch) continue;
        scope = frame;
        break;
      }
      if (!scope) return true;
      addImplicitEdge(state, scope.break);
      state.last = null;
      return true;
    }

    case NT.ContinueStmt: {
      let scope = null as LoopScope | null;
      for (let i = 0; i < state.stack.length; i++) {
        const frame = state.stack[state.stack.length - i - 1]!;
        if (frame.tag !== ST.Loop) continue;
        scope = frame;
        break;
      }
      if (!scope) return true;
      addImplicitEdge(state, scope.continue);
      state.last = null;
      return true;
    }

    case NT.ThrowStmt: {
      pushFlowNode(state, stmt);
      state.last = null;
      return true;
    }

    case NT.ReturnStmt: {
      const node = pushFlowNode(state, stmt);
      addFlowEdge(node, state.exit);
      state.last = null;
      return true;
    }

    case NT.IfStmt: {
      const elseCase = stmt.else;
      const base = pushFlowNode(state, stmt);
      const done = makeRawFlowNode('ifDone');

      state.last = [base, {tag: CT.If, expr: stmt.cond, value: true}];
      handleStmt(state, stmt.then);
      addImplicitEdge(state, done);

      state.last = [base, {tag: CT.If, expr: stmt.cond, value: false}];
      if (elseCase) handleStmt(state, elseCase);
      addImplicitEdge(state, done);

      state.last = [done, null];
      return true;
    }

    case NT.SwitchStmt: {
      const expr = stmt.expr;
      const base = pushFlowNode(state, stmt);
      const done = makeRawFlowNode('switchDone');

      state.last = null;
      state.stack.push({tag: ST.Switch, break: done});
      const length = state.stack.length;

      for (const c of stmt.cases) {
        const value = c.expr;
        const node = makeRawFlowNode(value ? 'switchCase' : 'defaultCase');
        addImplicitEdge(state, node);
        addFlowEdge(base, node, value ? {tag: CT.Switch, expr, value} : null);
        state.last = [node, null];
        handleStmt(state, c.then);
      }

      assert(state.stack.length === length);
      state.stack.pop();

      addImplicitEdge(state, done);
      state.last = [done, null];
      return true;
    }

    case NT.ForEachStmt:
    case NT.ForLoopStmt:
    case NT.WhileLoopStmt:
    case NT.DoWhileLoopStmt: {
      const head = makeRawFlowNode('loopHeader');
      const body = makeRawFlowNode('loopBody');
      const post = makeRawFlowNode('loopDone');

      let advance = null as FlowNode | null;
      if (stmt.tag === NT.ForLoopStmt) {
        advance = makeRawFlowNode('loopAdvance');
        handleStmt(state, stmt.init);
      }

      const test = stmt.tag === NT.ForEachStmt ? null :
                   stmt.tag === NT.ForLoopStmt ? stmt.cond.expr : stmt.cond;
      addImplicitEdge(state, stmt.tag === NT.DoWhileLoopStmt ? body : head);

      state.last = [head, null];
      const condStmt = stmt.tag === NT.ForLoopStmt ? stmt.cond : stmt;
      const cond = pushFlowNode(state, condStmt);
      addFlowEdge(cond, body, test ? {tag: CT.If, expr: test, value: true} : null);
      addFlowEdge(cond, post, test ? {tag: CT.If, expr: test, value: false} : null);

      state.last = [body, null];
      state.stack.push({tag: ST.Loop, break: post, continue: advance ?? head});
      const length = state.stack.length;
      handleStmt(state, stmt.body);
      assert(state.stack.length === length);
      state.stack.pop();

      if (stmt.tag === NT.ForLoopStmt) {
        addImplicitEdge(state, advance!);
        state.last = [advance!, null];
        handleStmt(state, stmt.post);
      }

      addImplicitEdge(state, head);
      state.last = [post, null];
      return true;
    }
  }
};

const printFlowGraph = (label: string | null, input: string, graph: FlowGraph): void => {
  const ordering = [] as FlowNode[];
  const visited = new Set() as Set<FlowNode>;

  const visit = (node: FlowNode): void => {
    if (visited.has(node)) return;
    visited.add(node);
    for (const edge of node.succs.slice().reverse()) visit(edge.succ);
    ordering.push(node); // post-order
  };

  visit(graph.entry);
  ordering.reverse(); // reverse post-order

  const indices = new Map() as Map<FlowNode, number>;
  for (let i = 0; i < ordering.length; i++) indices.set(ordering[i]!, i);

  console.log();
  console.log(`Control flow graph for: ${label ?? '<closure>'}:`);
  for (const node of ordering) {
    const index = indices.get(node) ?? 0;
    const label = node.label ?? (node.stmt ? formatNode(input, node.stmt) : '<unknown>');
    console.log(`  Node ${index}: ${label}`);
    for (const edge of node.succs) {
      const cond = edge.cond;
      const succ = edge.succ;
      const suffix = ((): string => {
        if (!cond) return '';
        return cond.tag === CT.Switch
            ? ` (case ${formatNode(input, cond.expr)} === ${getNodeText(input, cond.value)})`
            : ` (if ${cond.value}: ${formatNode(input, cond.expr)})`;
      })();
      console.log(`    -> Node ${indices.get(succ) ?? 0}${suffix}`);
    }
  }
  console.log();
};

const makeFlowGraph = (fn: ClosureExprNode, label: string | null, input: string): FlowGraph => {
  const entry = makeRawFlowNode('entry');
  const exit = makeRawFlowNode('exit');
  const graph = {entry, exit, nodes: new Map()} as FlowGraph;
  const state = {exit, last: [entry, null], graph, stack: []} as FlowState;

  handleStmt(state, fn.body);
  assert(state.stack.length === 0);
  addImplicitEdge(state, state.exit);

  const result = state.graph;
  if (true) printFlowGraph(label, input, result);
  return result;
};

return makeFlowGraph;

})();

//////////////////////////////////////////////////////////////////////////////

// Type-checking

type TypeCheck = (input: string, program: ProgramNode,
                  diagnostics: Diagnostic[], verbose: boolean) => void;

const typecheck = ((): TypeCheck => {

type TypeDeclNode = EnumDeclStmtNode | TypeDeclStmtNode;

type Variable = {
  binding: int,
  defined: boolean,
  mutable: boolean,
  type: Type,
};

type FlowType = Map<StmtNode, Type>;

type ClosureScope = {
  curStmt: StmtNode | null,
  flows: Map<Variable, FlowType>,
  graph: FlowGraph,
  label: string | null,
  type: ClosureType,
};

type Scope = {
  closure: ClosureScope | null,
  types: Map<string, Type>,
  variables: Map<string, Variable>,
};

type Env = {
  input: string,
  diagnostics: Diagnostic[],
  registry: TypeRegistry,
  bindings: Bindings,
  scopes: Scope[],
  verbose: boolean,

  // Used to resolve circular references between type aliases.
  typeDecls: Map<string, TypeDeclNode>,
  typeDeclStack: string[],
};

const error = (env: Env, node: Node, error: string): void => {
  env.diagnostics.push({pos: node.base.pos, error});
};

const union = (env: Env, node: Node, a: Type, b: Type): Type => {
  return typeUnion(a, b, env.registry, (x: string): void => error(env, node, x));
};

const nullable = (env: Env, type: Type): Type => {
  return typeUnion(env.registry.null, type, env.registry,
                   (x: string): void => assert(false));
};

const makeScope = (closure: ClosureScope | null = null): Scope => {
  const types = new Map() as Map<string, Type>;
  const variables = new Map() as Map<string, Variable>;
  return {closure, types, variables};
};

// Type declaration

const setType = (env: Env, name: string, type: Type): void => {
  env.scopes[0]!.types.set(name, type);
  if (env.verbose) {
    const pad = ' '.repeat(2 * env.scopes.length);
    console.log(`${pad}Type: ${name} => ${typeDescFull(type)}`);
  }
};

const setVariable = (env: Env, name: string, variable: Variable): void => {
  env.scopes[0]!.variables.set(name, variable);
  if (variable.defined && env.verbose) {
    const pad = ' '.repeat(2 * env.scopes.length);
    const mod = variable.mutable ? 'let' : 'const';
    const desc = env.scopes.length === 1 ? 'Global' : 'Local';
    console.log(`${pad}${desc}: ${mod} ${name} => ${typeDesc(variable.type)}`);
  }
};

const getTypeDeclNode = (stmt: StmtNode): TypeDeclNode | null => {
  if (stmt.tag === NT.EnumDeclStmt) return stmt;
  if (stmt.tag === NT.TypeDeclStmt) return stmt;
  return null;
};

const declareVariable = (env: Env, stmt: VariableDeclStmtNode): void => {
  const scope = env.scopes[0]!;
  const atGlobalScope = env.scopes.length === 1;
  const name = stmt.name.base.text;

  const errorCount = env.diagnostics.length;
  const type = atGlobalScope && stmt.expr.tag === NT.ClosureExpr
      ? typecheckClosureExpr(env, stmt.expr)
      : env.registry.error;
  env.diagnostics.length = errorCount;

  if (scope.variables.get(name)) {
    const desc = atGlobalScope ? 'global' : 'local';
    error(env, stmt, `Duplicate ${desc} declaration`);
  } else {
    const defined = atGlobalScope && stmt.expr.tag === NT.ClosureExpr;
    const mutable = stmt.keyword.base.text === 'let';
    setVariable(env, name, {binding: -1, defined, mutable, type});
  }
};

const declareType = (env: Env, node: TypeDeclNode): void => {
  const name = node.name.base.text;
  if (env.registry.primitives.has(name)) {
    error(env, node, 'Type alias cannot override primitive type');
    return;
  } else if (name === 'Map' || name === 'Set') {
    error(env, node, 'Type alias cannot override builtin type');
    return;
  } else if (env.typeDecls.has(name)) {
    error(env, node, 'Duplicate type declaration');
    return;
  }
  env.typeDecls.set(name, node);
};

const defineType = (env: Env, node: TypeDeclNode): boolean => {
  const name = node.name.base.text;
  const stack = env.typeDeclStack;
  const decls = env.typeDecls;
  if (!decls.has(name)) return true;

  for (let i = 0; i < stack.length; i++) {
    if (stack[i]! !== name) continue;
    const cycle = stack.slice(i).join(' -> ');
    error(env, node, `Circular type definition: ${cycle} -> ${name}`);
    return false;
  }

  if (node.tag === NT.EnumDeclStmt) {
    const values = new Set(node.values.map((x: Node): string => x.base.text));
    const result = {tag: TC.Enum, name, values} as EnumType;
    const struct = {tag: TC.Struct, name, fields: new Map()} as StructType;
    for (const value of values) {
      struct.fields.set(value, {tag: TC.Value, root: result, field: value});
    }
    setType(env, name, result);
    setVariable(env, name, {binding: -1, defined: true, mutable: false, type: struct});
    decls.delete(name);
    return true;
  }

  const rhs = node.type;
  if (rhs.tag !== NT.StructType) {
    stack.push(name);
    const type = resolveType(env, rhs, name);
    if (decls.has(name)) {
      setType(env, name, type);
      decls.delete(name);
    }
    stack.pop();
    return true;
  }

  const tmp = stack;
  env.typeDeclStack = [] as string[];
  const fields = new Map() as Map<string, Type>;
  const result = {tag: TC.Struct, name, fields} as StructType;
  env.scopes[0]!.types.set(name, result);
  decls.delete(name);

  for (const item of rhs.fields) {
    const fieldName = item.name.base.text;
    const fieldType = resolveType(env, item.type);
    if (!result.fields.has(fieldName)) {
      result.fields.set(fieldName, fieldType);
    } else {
      error(env, node, 'Duplicate struct field');
    }
  }
  assert(env.typeDeclStack.length === 0);
  setType(env, name, result);
  env.typeDeclStack = tmp;
  return true;
};

// Builtin types

type BuiltinArg = {name: string, type: Type, opt: boolean};

const resolveBuiltin = (argTypes: BuiltinArg[], result: Type): ClosureType => {
  const args = new Map() as Map<string, ArgType>;
  for (const at of argTypes) {
    args.set(at.name, {type: at.type, opt: at.opt});
  }
  assert(args.size === argTypes.length);
  return {tag: TC.Closure, args, result};
};

const arrayMethods = new Map([
  ['pop', (env: Env, array: ArrayType): ClosureType =>
      resolveBuiltin([], nullable(env, array.element))],
  ['push', (env: Env, array: ArrayType): ClosureType =>
      resolveBuiltin([{name: 'value', type: array.element, opt: false}],
                     env.registry.void)],
  ['shift', (env: Env, array: ArrayType): ClosureType =>
      resolveBuiltin([], nullable(env, array.element))],
  ['unshift', (env: Env, array: ArrayType): ClosureType =>
      resolveBuiltin([{name: 'value', type: array.element, opt: false}],
                     env.registry.void)],
  ['slice', (env: Env, array: ArrayType): ClosureType =>
      resolveBuiltin([{name: 'start', type: env.registry.dbl, opt: true},
                      {name: 'end', type: env.registry.dbl, opt: true}],
                      array)],
  ['join', (env: Env, array: ArrayType): ClosureType =>
      resolveBuiltin([{name: 'separator', type: env.registry.str, opt: true}],
                     env.registry.str)],
  ['sort', (env: Env, array: ArrayType): ClosureType => {
    const cmp = resolveBuiltin([{name: 'a', type: array.element, opt: false},
                                {name: 'b', type: array.element, opt: false}],
                               env.registry.dbl);
    return resolveBuiltin([{name: 'cmp', type: cmp, opt: true}], array);
  }],
]) as Map<string, (env: Env, array: ArrayType) => ClosureType>;

const mapMethods = new Map([
  ['$ctor', (env: Env, map: MapType): ClosureType => {
    const item = {tag: TC.Tuple, elements: [map.key, map.val]} as TupleType;
    const type = {tag: TC.Array, element: item} as ArrayType;
    return resolveBuiltin([{name: 'entries', type, opt: true}], map);
  }],
  ['clear', (env: Env, map: MapType): ClosureType =>
      resolveBuiltin([], env.registry.void)],
  ['delete', (env: Env, map: MapType): ClosureType =>
      resolveBuiltin([{name: 'key', type: map.key, opt: false}], env.registry.void)],
  ['has', (env: Env, map: MapType): ClosureType =>
      resolveBuiltin([{name: 'key', type: map.key, opt: false}], env.registry.bool)],
  ['get', (env: Env, map: MapType): ClosureType =>
      resolveBuiltin([{name: 'key', type: map.key, opt: false}],
                     nullable(env, map.val))],
  ['set', (env: Env, map: MapType): ClosureType =>
      resolveBuiltin([{name: 'key', type: map.key, opt: false},
                      {name: 'val', type: map.val, opt: false}],
                      env.registry.void)],
]) as Map<string, (env: Env, map: MapType) => ClosureType>;

const setMethods = new Map([
  ['$ctor', (env: Env, set: SetType): ClosureType => {
    const type = {tag: TC.Array, element: set.element} as ArrayType;
    return resolveBuiltin([{name: 'entries', type, opt: true}], set);
  }],
  ['clear', (env: Env, set: SetType): ClosureType =>
      resolveBuiltin([], env.registry.void)],
  ['delete', (env: Env, set: SetType): ClosureType =>
      resolveBuiltin([{name: 'value', type: set.element, opt: false}],
                     env.registry.void)],
  ['has', (env: Env, set: SetType): ClosureType =>
      resolveBuiltin([{name: 'value', type: set.element, opt: false}],
                     env.registry.bool)],
  ['add', (env: Env, set: SetType): ClosureType =>
      resolveBuiltin([{name: 'value', type: set.element, opt: false}],
                      env.registry.void)],
]) as Map<string, (env: Env, set: SetType) => ClosureType>;

const strMethods = new Map([
  ['repeat', (env: Env): ClosureType =>
      resolveBuiltin([{name: 'count', type: env.registry.dbl, opt: false}],
                      env.registry.str)],
  ['substring', (env: Env): ClosureType =>
      resolveBuiltin([{name: 'start', type: env.registry.dbl, opt: false},
                      {name: 'end', type: env.registry.dbl, opt: true}],
                      env.registry.str)],
]) as Map<string, (env: Env) => ClosureType>;

// Type resolution

const resolveType =
    (env: Env, type: TypeNode, name: string | null = null): Type => {
  switch (type.tag) {
    case NT.IdentifierType: {
      const name = type.base.text;
      const prim = env.registry.primitives.get(name) ?? null;
      if (prim) return prim;

      const decl = env.typeDecls.get(name) ?? null;
      if (decl && !defineType(env, decl)) return env.registry.error;

      for (const scope of env.scopes) {
        const result = scope.types.get(name) ?? null;
        if (result) return result;
      }
      error(env, type, `Unknown type or type alias: '${name}'`);
      return env.registry.error;
    }
    case NT.ArrayType: {
      return {tag: TC.Array, element: resolveType(env, type.element)};
    }
    case NT.ErrorType: {
      return env.registry.error;
    }
    case NT.TupleType: {
      const elements = type.elements.map(
          (x: TypeNode): Type => resolveType(env, x));
      return {tag: TC.Tuple, elements};
    }
    case NT.UnionType: {
      let error = false;
      let result = env.registry.never as Type;
      for (const option of type.options) {
        const here = resolveType(env, option);
        const next = union(env, option, result, here);
        if (next.tag !== TC.Error) result = next;
        if (next.tag === TC.Error) error = true;
      }
      if (name && result.tag === TC.Union) result.name = name;
      return error ? env.registry.error : result;
    }
    case NT.ValueType: {
      const name = type.root.base.text;
      let root = null as Type | null;
      for (const scope of env.scopes) {
        root = scope.types.get(name) ?? null;
        if (root) break;
      }
      if (!root) {
        error(env, type, `Unknown enum type: '${name}'`);
        return env.registry.error;
      } else if (root.tag !== TC.Enum) {
        error(env, type, 'Only enum-valued type literals are supported');
        return env.registry.error;
      }
      return {tag: TC.Value, root, field: type.field.base.text};
    }
    case NT.StructType: {
      error(env, type, 'Structs are nominal and need a top-level definition');
      return env.registry.error;
    }
    case NT.ClosureType: {
      let foundOpt = false;
      const args = new Map() as Map<string, ArgType>;
      for (const arg of type.args) {
        const name = arg.name.base.text;
        const type = resolveType(env, arg.type);
        if (!args.has(name)) {
          if (arg.opt) {
            foundOpt = true;
          } else if (foundOpt) {
            error(env, arg, `Non-optional arg cannot come after optional args`);
          }
          args.set(name, {type, opt: !!arg.opt});
        } else {
          error(env, arg, 'Duplicate closure argument');
        }
      }
      return {tag: TC.Closure, args, result: resolveType(env, type.result)};
    }
    case NT.GenericType: {
      const name = type.name.base.text;
      if (name === 'Map') {
        if (type.args.length !== 2) {
          error(env, type, 'Map takes two generic type arguments');
          return env.registry.error;
        }
        const key = resolveType(env, type.args[0]!);
        const val = resolveType(env, type.args[1]!);
        return {tag: TC.Map, key, val};
      } else if (name === 'Set') {
        if (type.args.length !== 1) {
          error(env, type, 'Set takes one generic type argument');
          return env.registry.error;
        }
        return {tag: TC.Set, element: resolveType(env, type.args[0]!)};
      }
      error(env, type, 'User-defined generic types are not yet supported');
      return env.registry.error;
    }
  }
};

// Control-flow-sensitive type-checking

const matchVariable = (env: Env, expr: ExprNode, binding: int): boolean => {
  return expr.tag === NT.VariableExpr && env.bindings.indices.get(expr) === binding;
};

const matchTagField = (env: Env, expr: ExprNode, binding: int): boolean => {
  if (expr.tag !== NT.FieldAccessExpr) return false;
  return matchVariable(env, expr.root, binding) && expr.field.base.text === 'tag';
};

const matchTagValue = (env: Env, expr: ExprNode): ValueType | null => {
  if (expr.tag !== NT.FieldAccessExpr) return null;

  const root = expr.root;
  const field = expr.field.base.text;
  if (root.tag !== NT.VariableExpr) return null;

  for (const scope of env.scopes) {
    const type = scope.types.get(root.base.text);
    if (!(type && type.tag === TC.Enum)) continue;
    const value = scope.variables.get(root.base.text);
    if (!value) continue;
    const valueType = value.type;
    if (valueType.tag !== TC.Struct) continue;
    const valueValue = valueType.fields.get(field);
    if (!(valueValue && valueValue.tag === TC.Value)) continue;
    return valueValue;
  }
  return null;
};

const maybeFalsy = (type: Type): boolean => {
  switch (type.tag) {
    case TC.Dbl: return true;
    case TC.Bool: return true;
    case TC.Null: return true;
    case TC.Void: return true;
    case TC.Error: return true;
    case TC.Value: return true;
    case TC.Nullable: return true;

    case TC.Exn: return false;
    case TC.Str: return false;
    case TC.Never: return false;
    case TC.Array: return false;
    case TC.Tuple: return false;
    case TC.Union: return false;
    case TC.Struct: return false;
    case TC.Closure: return false;
    case TC.Enum: return false;
    case TC.Map: return false;
    case TC.Set: return false;
  }
};

const applyEquality = (env: Env, lhs: ExprNode, rhs: ExprNode, equal: boolean,
                       binding: int, type: Type): Type => {
  if (matchVariable(env, lhs, binding) && rhs.tag === NT.NullLiteralExpr) {
    return equal ? env.registry.null : typeWithoutNull(type, env.registry);
  }

  if (type.tag === TC.Union && matchTagField(env, lhs, binding)) {
    const rt = matchTagValue(env, rhs);
    if (!rt) return type;

    const filtered = [] as StructType[];
    for (const struct of type.options) {
      const lt = struct.fields.get('tag')!;
      const match = lt.tag === TC.Value && lt.root === rt.root && lt.field === rt.field;
      if (equal === match) filtered.push(struct);
    }
    if (filtered.length === type.options.length) return type;
    if (filtered.length === 0) return env.registry.never;
    if (filtered.length === 1) return filtered[0]!;
    return {tag: TC.Union, name: null, options: filtered};
  }
  return type;
};

const applyIf = (env: Env, expr: ExprNode, value: boolean, binding: int, type: Type): Type => {
  if (matchVariable(env, expr, binding)) {
    if (value) return typeWithoutNull(type, env.registry);
    const restrictToNull = type.tag === TC.Nullable && !maybeFalsy(type.base);
    return restrictToNull ? env.registry.null : type;
  }

  if (expr.tag === NT.UnaryPrefixOpExpr) {
    return expr.op.base.text === '!' ? applyIf(env, expr.expr, !value, binding, type) : type;
  }

  if (expr.tag === NT.BinaryOpExpr) {
    const lhs = expr.lhs;
    const rhs = expr.rhs;
    const op = expr.op.base.text;
    if (op !== '===' && op !== '!==') return type;

    const equal = value === (op === '===');
    return applyEquality(env, lhs, rhs, equal, binding, type);
  }
  return type;
};

const applyFlowCond = (env: Env, cond: FlowCond, binding: int, type: Type): Type => {
  switch (cond.tag) {
    case CT.If: {
      const result = applyIf(env, cond.expr, cond.value, binding, type);
      return result;
    }
    case CT.Switch: return applyEquality(env, cond.expr, cond.value, true, binding, type);
  }
};

const getFlowType = (env: Env, frame: ClosureScope, type: Type, variable: Variable): FlowType => {
  const cached = frame.flows.get(variable);
  if (cached) return cached;

  const graph = frame.graph;
  const nodes = new Map() as Map<FlowNode, Type>;
  const dirty = new Set() as Set<FlowNode>;
  nodes.set(graph.entry, type);
  dirty.add(graph.entry);

  while (dirty.size > 0) {
    const node = ((): FlowNode => {
      for (const node of dirty.values()) {
        return node;
      }
      throw new Error();
    })();
    dirty.delete(node);

    const incoming = nodes.get(node)!;
    for (const edge of node.succs) {
      const cond = edge.cond;
      const outgoing = cond ? applyFlowCond(env, cond, variable.binding, incoming) : incoming;
      const oldSuccType = nodes.get(edge.succ) ?? env.registry.never;
      const newSuccType = typeUnion(oldSuccType, outgoing, env.registry, (x: string): void => {});
      if (typeMatches(oldSuccType, newSuccType)) continue;

      nodes.set(edge.succ, newSuccType);
      dirty.add(edge.succ);
    }
  }

  const result = new Map() as FlowType;
  for (const entry of nodes.entries()) {
    const stmt = entry[0].stmt;
    if (stmt) result.set(stmt, entry[1]);
  }
  frame.flows.set(variable, result);
  return result;
};

const maybeRefineType = (env: Env, expr: VariableExprNode, variable: Variable): Type => {
  let type = variable.type;
  if (type.tag !== TC.Nullable && type.tag !== TC.Union) return type;

  const binding = env.bindings.indices.get(expr) ?? null;
  if (binding === null) throw new Error();
  if (variable.binding !== -1 && variable.binding !== binding) throw new Error();

  variable.binding = binding;
  if (variable.mutable && env.bindings.mutated[binding]) return type;

  const frames = [] as ClosureScope[];
  for (const scope of env.scopes) {
    const closure = scope.closure;
    if (closure) frames.push(closure);
  }
  frames.reverse();

  for (const frame of frames) {
    const stmt = frame.curStmt;
    const flow = getFlowType(env, frame, type, variable);
    if (stmt) type = flow.get(stmt) ?? type;
  }
  return type;
};

// Type checking statements

const resolveVariable = (env: Env, expr: VariableExprNode): Variable | null => {
    const name = expr.base.text;
    for (const scope of env.scopes) {
      const variable = scope.variables.get(name);
      if (variable) return variable;
    }
    return null;
};

const typecheckFieldAccess = (env: Env, type: Type, expr: FieldAccessExprNode,
                              field: string, called: boolean): Type => {
  switch (type.tag) {
    // Collections:
    case TC.Array: {
      if (field === 'length') return env.registry.dbl;
      const method = arrayMethods.get(field);
      if (!method) break;
      if (!called) error(env, expr.field, `Method ${field} must be called.`);
      if (field === 'join' && !typeAccepts(env.registry.str, type.element)) {
        error(env, expr.root, `join must be called on an array of strings!`);
      }
      return method(env, type);
    }
    case TC.Map: {
      if (field === 'size') return env.registry.dbl;
      const method = mapMethods.get(field);
      if (!method) break;
      if (!called) error(env, expr.field, `Method ${field} must be called.`);
      return method(env, type);
    }
    case TC.Set: {
      if (field === 'size') return env.registry.dbl;
      const method = setMethods.get(field);
      if (!method) break;
      if (!called) error(env, expr.field, `Method ${field} must be called.`);
      return method(env, type);
    }
    // Other cases:
    case TC.Str: {
      if (field === 'length') return env.registry.dbl;
      const method = strMethods.get(field);
      if (!method) break;
      if (!called) error(env, expr.field, `Method ${field} must be called.`);
      return method(env);
    }
    case TC.Struct: {
      const fieldType = type.fields.get(field) ?? null;
      if (fieldType) return fieldType;
      break;
    }
    case TC.Union: {
      let okay = true;
      let result = env.registry.never as Type;
      for (const option of type.options) {
        const fieldType = option.fields.get(field) ?? null;
        okay = fieldType !== null;
        if (!fieldType) break;
        result = union(env, expr, result, fieldType);
      }
      if (okay) return result;
      break;
    }
  }
  error(env, expr.field, `Field ${field} does not exist on ${typeDesc(type)}`);
  return env.registry.error;
};

const typecheckClosureExpr = (env: Env, expr: ClosureExprNode): ClosureType => {
  let foundOpt = false;
  const args = new Map() as Map<string, ArgType>;
  for (const arg of expr.args) {
    const name = arg.name.base.text;
    const type = resolveType(env, arg.type);
    if (!args.has(name)) {
      if (arg.def) {
        foundOpt = true;
      } else if (foundOpt) {
        error(env, arg, `Non-optional arg cannot come after optional args`);
      }
      args.set(name, {opt: !!arg.def, type});
    } else {
      error(env, arg, 'Duplicate closure argument');
    }
  }
  const result = resolveType(env, expr.result);
  return {tag: TC.Closure, args, result};
};

const typecheckCallExpr =
    (env: Env, root: Node, args: ExprNode[], fn: ClosureType): Type => {
  const expected = Array.from(fn.args.values());
  const min = expected.filter((x: ArgType): boolean => !x.opt).length;
  const max = expected.length;

  if (!(min <= args.length && args.length <= max)) {
    const count = min === max ? `${min}` : `${min}-${max}`;
    error(env, root, `Expected: ${count} arg(s); got: ${args.length}`);
  }

  for (let i = 0; i < args.length; i++) {
    const hint = i < expected.length ? expected[i]!.type : null;
    const type = typecheckExpr(env, args[i]!, hint);
    if (!hint || typeAccepts(hint, type)) continue;
    error(env, args[i]!, `Expected: ${typeDesc(hint)}; got: ${typeDesc(type)}`);
  }
  return fn.result;
};

const typecheckCondExpr = (env: Env, expr: ExprNode, type: Type): void => {
  if (typeAccepts(env.registry.bool, type)) return;
  if (type.tag === TC.Nullable || typeAccepts(env.registry.dbl, type)) return;
  error(env, expr, `Expected: boolean, number, or nullable type: got: ${typeDesc(type)}`);
};

const typecheckTagExpr = (env: Env, expr: ExprNode): ValueType | null => {
  const errorCount = env.diagnostics.length;
  if (expr.tag !== NT.FieldAccessExpr) return null;
  if (expr.root.tag !== NT.VariableExpr) return null;
  const result = typecheckExprAllowVoid(env, expr);
  env.diagnostics.length = errorCount;
  return result.tag === TC.Value ? result : null;
};

const typecheckExpr =
    (env: Env, expr: ExprNode, hint: Type | null = null,
     label: string | null = null, called: boolean = false): Type => {
  const result = typecheckExprAllowVoid(env, expr, hint, label, called);
  if (result.tag !== TC.Void) return result;
  error(env, expr, 'Usage of void return type');
  return env.registry.error;
};

const typecheckExprAllowVoid =
    (env: Env, expr: ExprNode, hint: Type | null = null,
     label: string | null = null, called: boolean = false): Type => {
  const unhandled = (): Type => {
    error(env, expr, `Unhandled ${NT[expr.tag]}: ${expr.base.text}`);
    return env.registry.error;
  };

  switch (expr.tag) {
    // Trivial cases:
    case NT.ErrorExpr: return env.registry.error;
    case NT.DblLiteralExpr: return env.registry.dbl;
    case NT.IntLiteralExpr: return env.registry.dbl;
    case NT.StrLiteralExpr: return env.registry.str;
    case NT.BoolLiteralExpr: return env.registry.bool;
    case NT.NullLiteralExpr: return env.registry.null;

    // In progress:
    case NT.VariableExpr: {
      const name = expr.base.text;
      const variable = resolveVariable(env, expr);
      if (!variable) {
        error(env, expr, `Unknown variable '${name}'`);
        return env.registry.error;
      } else if (!variable.defined) {
        error(env, expr, `Variable '${name}' used before it was defined`);
      }
      return maybeRefineType(env, expr, variable);
    }
    case NT.AssignmentExpr: {
      const lhs = typecheckExpr(env, expr.lhs);
      const rhs = typecheckExpr(env, expr.rhs, lhs);
      const dbl = env.registry.dbl;

      if (expr.lhs.tag !== NT.VariableExpr &&
          expr.lhs.tag !== NT.FieldAccessExpr &&
          expr.lhs.tag !== NT.IndexAccessExpr) {
        error(env, expr.lhs, `LHS must be a variable, struct field, or array element`);
      }

      const target = expr.lhs;
      if (target.tag === NT.VariableExpr) {
        const variable = resolveVariable(env, target);
        if (variable && !variable.mutable) {
          error(env, expr.lhs, `Cannot assign const variable '${target.base.text}'`);
        }
      }

      if (!typeAccepts(lhs, rhs)) {
        error(env, expr.rhs, `Expected: ${typeDesc(lhs)}; got: ${typeDesc(rhs)}`);
      } else if (expr.op.base.text !== '=' && !typeAccepts(dbl, lhs)) {
        error(env, expr.rhs, `Expected: ${typeDesc(dbl)}; got: ${typeDesc(lhs)}`);
      }
      return lhs;
    }
    case NT.BinaryOpExpr: {
      const op = expr.op.base.text;
      const lhs = typecheckExpr(env, expr.lhs);
      const rhs = typecheckExpr(env, expr.rhs);
      const ld = (): string => typeDesc(lhs);
      const rd = (): string => typeDesc(rhs);

      // Relevant types:
      const dbl = env.registry.dbl;
      const str = env.registry.str;
      const err = env.registry.error;
      const bool = env.registry.bool;

      if (op === '+' || op === '-' || op === '*' || op === '/') {
        if (typeAccepts(dbl, lhs) && typeAccepts(dbl, rhs)) return dbl;
        if (op === '+' && typeAccepts(str, lhs) && typeAccepts(str, rhs)) return str;
        error(env, expr, `Invalid arithmetic op ${op} between: ${ld()} and ${rd()}`);
        return err;
      } else if (op === '<' || op === '>' || op === '<=' || op === '>=') {
        if (typeAccepts(dbl, lhs) && typeAccepts(dbl, rhs)) return bool;
        if (typeAccepts(str, lhs) && typeAccepts(str, rhs)) return bool;
        error(env, expr, `Invalid comparison op ${op} between: ${ld()} and ${rd()}`);
        return bool;
      } else if (op === '===' || op === '!==') {
        if (typesCompareEqual(lhs, rhs)) return bool;
        error(env, expr, `Invalid equality check ${op} between: ${ld()} and ${rd()}`);
        return bool;
      } else if (op === '==' || op == '!=') {
        error(env, expr, `Weak equality is unsupported. Use ${op}= instead.`);
        return bool;
      } else if (op === '&&' || op == '||') {
        typecheckCondExpr(env, expr.lhs, lhs);
        typecheckCondExpr(env, expr.rhs, lhs);
        return union(env, expr, lhs, rhs);
      } else if (op === '??') {
        if (lhs.tag === TC.Error) return lhs;
        if (lhs.tag !== TC.Nullable) {
          error(env, expr.lhs, `Expected: nullable type; got: ${ld()}`);
          return err;
        } else if (!typeAccepts(lhs, rhs)) {
          error(env, expr.rhs, `Expected: ${ld()}; got: ${rd()}`);
          return err;
        }
        return typeAccepts(lhs.base, rhs) ? lhs.base : lhs;
      }
      return unhandled();
    }
    case NT.UnaryPrefixOpExpr: {
      const op = expr.op.base.text;
      const base = typecheckExpr(env, expr.expr);
      if (op === '!') {
        typecheckCondExpr(env, expr.expr, base);
        return env.registry.bool;
      } else if (op === '-' || op === '++' || op === '--') {
        const dbl = env.registry.dbl;
        if (typeAccepts(dbl, base)) return dbl;
        error(env, expr.expr, `Expected: numeric type; got: ${typeDesc(base)}`);
        return env.registry.error;
      }
      return unhandled();
    }
    case NT.UnarySuffixOpExpr: {
      const op = expr.op.base.text;
      const base = typecheckExpr(env, expr.expr);
      if (op === '!') {
        if (base.tag === TC.Error) return base;
        if (base.tag === TC.Nullable) return base.base;
        error(env, expr.expr, `Expected: nullable value; got: ${typeDesc(base)}`);
        return base;
      } else if (op === '++' || op === '--') {
        const dbl = env.registry.dbl;
        if (typeAccepts(dbl, base)) return dbl;
        error(env, expr.expr, `Expected: numeric type; got: ${typeDesc(base)}`);
        return env.registry.error;
      }
      return unhandled();
    }
    case NT.CastExpr: {
      const hint = resolveType(env, expr.type);
      const type = typecheckExpr(env, expr.expr, hint);
      if (!typeAccepts(hint, type)) {
        error(env, expr.expr, `Expected: ${typeDesc(hint)}; got: ${typeDesc(type)}`);
      }
      return hint;
    }
    case NT.ArrayExpr: {
      if (!hint || (hint.tag !== TC.Array && hint.tag !== TC.Tuple)) {
        error(env, expr, `Could not infer array literal type. Use an 'as T[]' cast.`);
        return {tag: TC.Array, element: env.registry.error};
      } else if (hint.tag === TC.Tuple) {
        const ee = expr.elements.length;
        const he = hint.elements.length;
        if (ee !== he) error(env, expr, `Expected: ${he} elements; got: ${ee}`);
        for (let i = 0; i < ee; i++) {
          const here = expr.elements[i]!;
          const base = i < he ? hint.elements[i]! : null;
          const type = typecheckExpr(env, here, base);
          if (base && !typeAccepts(base, type)) {
            error(env, here, `Expected: ${typeDesc(base)}; got: ${typeDesc(type)}`);
          }
        }
        return hint;
      }
      const element = hint.element;
      let desc = null as string | null;
      for (const value of expr.elements) {
        const type = typecheckExpr(env, value, element);
        if (!typeAccepts(element, type)) {
          desc = desc ? desc : typeDesc(element);
          error(env, value, `Expected: ${desc}; got: ${typeDesc(type)}`);
        }
      }
      return hint;
    }
    case NT.StructExpr: {
      const struct = ((hint: Type | null): StructType | null => {
        if (!hint) return null;
        if (hint.tag === TC.Nullable) hint = hint.base;
        if (hint.tag === TC.Union) {
          const tags = expr.fields.filter(
              (x: FieldExprNode): boolean => x.name.base.text === 'tag');
          if (tags.length === 0) return null;
          const tag = typecheckTagExpr(env, tags[0]!.expr);
          if (!tag) return null;
          const matches = hint.options.filter((x: StructType): boolean => {
            const option = x.fields.get('tag');
            return option ? typeMatches(option, tag) : false;
          });
          if (matches.length === 1) hint = matches[0]!;
        }
        return hint.tag === TC.Struct ? hint : null;
      })(hint);

      if (!struct) {
        error(env, expr, `Struct type must be annotated`);
        for (const field of expr.fields) typecheckExpr(env, field.expr);
        return env.registry.error;
      }

      const expected = new Set(struct.fields.keys());
      for (const field of expr.fields) {
        const name = field.name.base.text;
        const hint = struct.fields.get(name);
        if (!hint) {
          error(env, field.name, `Field ${name} does not exist on ${struct.name}`);
          typecheckExpr(env, field.expr);
        } else {
          if (!expected.has(name)) error(env, field.name, `Duplicate field`);
          const type = typecheckExpr(env, field.expr, hint);
          if (!typeAccepts(hint, type)) {
            const hd = typeDesc(hint);
            const td = typeDesc(type);
            error(env, field.expr, `${struct.name}.${name} must be a ${hd}; got: ${td}`);
          }
          expected.delete(name);
        }
      }
      if (expected.size > 0) {
        const missing = Array.from(expected.values()).join(', ');
        error(env, expr, `Missing ${struct.name} field(s): ${missing}`);
      }
      return struct;
    }
    case NT.ClosureExpr: {
      const type = typecheckClosureExpr(env, expr);
      const graph = makeFlowGraph(expr, label, env.input);
      const scope = {curStmt: null, flows: new Map(), graph, label, type} as ClosureScope;
      typecheckBlock(env, expr.body.stmts, makeScope(scope));
      return type;
    }
    // Various kinds of function calls:
    case NT.FunctionCallExpr: {
      const fn = typecheckExpr(env, expr.fn, null, null, true);
      if (fn.tag === TC.Error) return env.registry.error;
      if (fn.tag !== TC.Closure) {
        error(env, expr.fn, `Called non-function: ${typeDesc(fn)}`);
        return env.registry.error;
      }
      return typecheckCallExpr(env, expr.fn, expr.args.args, fn);
    }
    case NT.ConstructorCallExpr: {
      const cls = expr.cls.base.text;
      const args = expr.args.args;
      if (cls === 'Error') {
        const bs = [{name: 'message', type: env.registry.str, opt: true}] as BuiltinArg[];
        const fn = resolveBuiltin(bs, env.registry.exn);
        return typecheckCallExpr(env, expr.cls, args, fn);
      } else if (cls === 'Map') {
        if (!hint || hint.tag !== TC.Map) {
          args.map((x: ExprNode): Type => typecheckExpr(env, x));
          return {tag: TC.Map, key: env.registry.error, val: env.registry.error};
        }
        const fn = mapMethods.get('$ctor')!(env, hint);
        return typecheckCallExpr(env, expr.cls, args, fn);
      } else if (cls === 'Set') {
        if (!hint || hint.tag !== TC.Set) {
          args.map((x: ExprNode): Type => typecheckExpr(env, x));
          return {tag: TC.Set, element: env.registry.error};
        }
        const fn = setMethods.get('$ctor')!(env, hint);
        return typecheckCallExpr(env, expr.cls, args, fn);
      }
      return unhandled();
    }
    case NT.FieldAccessExpr: {
      const field = expr.field.base.text;
      let type = typecheckExpr(env, expr.root);
      while (true) {
        if (type.tag === TC.Error) return env.registry.error;
        if (type.tag === TC.Nullable) {
          error(env, expr.root, `Value may be null: ${typeDesc(type)}`);
          type = type.base;
          continue;
        }
        break;
      }
      return typecheckFieldAccess(env, type, expr, field, called);
    }
    case NT.IndexAccessExpr: {
      const root = typecheckExpr(env, expr.root);
      const index = typecheckExpr(env, expr.index);

      // Tuple indices are a special case: they must be literals.
      if (root.tag === TC.Tuple) {
        if (expr.index.tag !== NT.IntLiteralExpr) {
          error(env, expr.index, `Tuple indices must be integer literals.`);
          return env.registry.error;
        }
        const elements = root.elements;
        const index = parseInt(expr.index.base.text, 10);
        if (!(0 <= index && index < elements.length)) {
          error(env, expr.index, `Tuple index must be in [0, ${elements.length} - 1]`);
          return env.registry.error;
        }
        return elements[index]!;
      }

      // Array or string indices share the rest of the code below.
      if (!typeAccepts(env.registry.dbl, index)) {
        error(env, expr.index, `Expected: index; got: ${typeDesc(index)}`);
      }
      if (root.tag === TC.Error) return env.registry.error;
      if (root.tag === TC.Array) return nullable(env, root.element);
      if (root.tag === TC.Str)   return nullable(env, env.registry.str);
      error(env, expr.root, `Expected: array or string; got: ${typeDesc(root)}`);
      return env.registry.error;
    }
    case NT.TernaryExpr: {
      const cond = typecheckExpr(env, expr.cond);
      typecheckCondExpr(env, expr.cond, cond);
      const lhs = typecheckExpr(env, expr.lhs);
      const rhs = typecheckExpr(env, expr.rhs);
      return union(env, expr, lhs, rhs);
    }
    case NT.TemplateExpr: {
      for (const part of expr.suffixes) {
        let type = typecheckExpr(env, part[0]);
        if (type.tag === TC.Error || type.tag === TC.Null) continue;
        const tag = type.tag === TC.Nullable ? type.base.tag : type.tag;
        if (tag == TC.Bool || tag == TC.Dbl || tag === TC.Str) continue;
        error(env, part[0], `Template parts be primitives; got: ${typeDesc(type)}`);
      }
      return env.registry.str;
    }
  }
};

const typecheckStmt = (env: Env, stmt: StmtNode): void => {
  const closure = ((): ClosureScope | null => {
    for (const scope of env.scopes) {
      const closure = scope.closure;
      if (closure) return closure;
    }
    return null;
  })();
  if (!closure) {
    typecheckStmtHelper(env, stmt);
    return;
  }

  const prevStmt = closure.curStmt;
  closure.curStmt = stmt;
  typecheckStmtHelper(env, stmt);
  closure.curStmt = prevStmt;
};

const typecheckStmtHelper = (env: Env, stmt: StmtNode): boolean => {
  switch (stmt.tag) {
    // Trivial cases:
    case NT.EmptyStmt: return true;
    case NT.BreakStmt: return true;
    case NT.ContinueStmt: return true;
    case NT.EnumDeclStmt: return true;
    case NT.TypeDeclStmt: return true;
    case NT.ExternDeclStmt: return true;

    case NT.ExprStmt: {
      typecheckExprAllowVoid(env, stmt.expr, null);
      return true;
    }
    case NT.ThrowStmt: {
      const type = typecheckExpr(env, stmt.expr);
      if (!typeAccepts(env.registry.exn, type)) {
          error(env, stmt.expr, `Expected: Error; got: ${typeDesc(type)}`);
      }
      return true;
    }
    case NT.BlockStmt: {
      typecheckBlock(env, stmt.stmts, makeScope());
      return true;
    }
    case NT.IfStmt: {
      const elseCase = stmt.else;
      const type = typecheckExpr(env, stmt.cond);
      typecheckCondExpr(env, stmt.cond, type);
      typecheckStmt(env, stmt.then);
      if (elseCase) typecheckStmt(env, elseCase);
      return true;
    }
    case NT.ReturnStmt: {
      const result = ((): Type | null => {
        for (const x of env.scopes) if (x.closure) return x.closure.type.result;
        return null;
      })();
      const type = stmt.expr ? typecheckExpr(env, stmt.expr, result) : env.registry.void;
      if (!result) {
        error(env, stmt, 'Return statement outside of a closure');
      } else if (!typeAccepts(result, type)) {
        const node = stmt.expr ? stmt.expr : stmt;
        error(env, node, `Expected: ${typeDesc(result)}; got: ${typeDesc(type)}`);
      }
      return true;
    }
    case NT.VariableDeclStmt: {
      const name = stmt.name.base.text;
      const variable = env.scopes[0]!.variables.get(name)!;

      // Define the variable early if it's a closure to allow self-recursion.
      // Mutual recursion only works for global functions, which are hoisted.
      const defined = variable.defined;
      if (stmt.expr.tag === NT.ClosureExpr) variable.defined = true;

      // Type-check the expression, even if it's an (unused) re-declaration.
      const type = typecheckExpr(env, stmt.expr, null, name);
      if (defined) return true;

      variable.defined = true;
      variable.type = type;
      setVariable(env, name, variable);
      return true;
    }
    case NT.SwitchStmt: {
      const lhs = typecheckExpr(env, stmt.expr);
      for (const c of stmt.cases) {
        const expr = c.expr;
        if (expr) {
          const rhs = typecheckExpr(env, expr);
          if (!typesCompareEqual(lhs, rhs)) {
            error(env, expr, `Invalid switch case: ${typeDesc(lhs)} vs. ${typeDesc(rhs)}`);
          }
        }
        typecheckStmt(env, c.then);
      }
      return true;
    }
    case NT.ForEachStmt: {
      const name = stmt.name.base.text;
      const base = typecheckExpr(env, stmt.expr);
      const type = ((): Type => {
        if (base.tag === TC.Error) return env.registry.error;
        if (base.tag === TC.Array) return base.element;
        if (base.tag === TC.Set) return base.element;
        if (base.tag === TC.Map) return {tag: TC.Tuple, elements: [base.key, base.val]};
        error(env, stmt.expr, `For-each over non-iterable type: ${typeDesc(base)}`);
        return env.registry.error;
      })();

      const scope = makeScope();
      const mutable = stmt.keyword.base.text === 'let';
      scope.variables.set(name, {binding: -1, defined: true, mutable, type});
      typecheckBlock(env, [stmt.body], scope);
      return true;
    }
    case NT.ForLoopStmt: {
      const block = [stmt.init, stmt.cond, stmt.body, stmt.post] as StmtNode[];
      typecheckBlock(env, block, makeScope());
      return true;
    }
    case NT.WhileLoopStmt: {
      const type = typecheckExpr(env, stmt.cond);
      typecheckCondExpr(env, stmt.cond, type);
      typecheckStmt(env, stmt.body);
      return true;
    }
    case NT.DoWhileLoopStmt: {
      typecheckStmt(env, stmt.body);
      const type = typecheckExpr(env, stmt.cond);
      typecheckCondExpr(env, stmt.cond, type);
      return true;
    }
  }
};

const typecheckBlock = (env: Env, block: StmtNode[], scope: Scope): void => {
  if (env.verbose) {
    const closure = scope.closure;
    const pad = ' '.repeat(2 * env.scopes.length);
    const label = !closure ? env.scopes.length ? '<block>' : '<global>'
                           : closure.label ?? '<closure>';
    console.log(`${pad}Scope: ${label} {`);
  }
  env.scopes.unshift(scope);

  const closure = scope.closure;
  if (closure) {
    for (const entry of closure.type.args.entries()) {
      const type = entry[1].type;
      setVariable(env, entry[0], {binding: -1, defined: true, mutable: true, type});
    }
  } else {
    for (const entry of scope.variables.entries()) {
      setVariable(env, entry[0], entry[1]);
    }
  }

  assert(env.typeDecls.size === 0);
  assert(env.typeDeclStack.length === 0);
  const decls = [] as TypeDeclNode[];
  for (const stmt of block) {
    const decl = getTypeDeclNode(stmt);
    if (decl) decls.push(decl);
  }
  for (const decl of decls) declareType(env, decl);
  for (const decl of decls) defineType(env, decl);
  assert(env.typeDecls.size === 0);
  assert(env.typeDeclStack.length === 0);

  for (const stmt of block) {
    if (stmt.tag !== NT.VariableDeclStmt) continue;
    declareVariable(env, stmt);
  }
  for (const stmt of block) typecheckStmt(env, stmt);

  env.scopes.shift()!;
  if (env.verbose) console.log(`${' '.repeat(2 * env.scopes.length)}}`);
};

const typecheck = (input: string, program: ProgramNode,
                   diagnostics: Diagnostic[], verbose: boolean): void => {
  const env = {
    input,
    diagnostics,
    registry: makeTypeRegistry(),
    bindings: resolveBindings(input, program),
    scopes: [],
    typeDecls: new Map(),
    typeDeclStack: [],
    verbose,
  } as Env;
  typecheckBlock(env, program.stmts, makeScope());
  assert(env.scopes.length === 0);
};

return typecheck;

})();

//////////////////////////////////////////////////////////////////////////////

// Entry point

declare const console: {log: any};
declare const process: {argv: string[]};
declare const require: (x: string) => any;

const main = (): void => {
  const fs = require('fs');
  const args = (process.argv as string[]).slice();
  if (!(args.length === 3 || (args.length === 4 && args[3] === '-v'))) {
    throw new Error(`Usage: node ${args[1]} $FILE [-v]`);
  }

  let verbose = args.length > 3;
  const input = fs.readFileSync(args[2], 'utf8');
  const diagnostics = [] as Diagnostic[];
  const program = parse(input, diagnostics);
  if (verbose) console.log(formatAST(input, program));
  if (diagnostics.length > 0) {
    if (verbose) console.log();
    console.log(formatDiagnostics(input, diagnostics, true));
    return;
  }

  verbose = true;
  typecheck(input, program, diagnostics, verbose);
  if (diagnostics.length > 0) {
    if (verbose) console.log();
    console.log(formatDiagnostics(input, diagnostics, true));
  }
};

main();
