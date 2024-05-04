#![feature(let_chains)]

use std::collections::{HashMap, HashSet};

struct Error<'a> {
    error: String,
    pos: &'a str,
}

// Keywords and Symbols

fn keywords() -> HashSet<&'static str> {
    let mut result = HashSet::default();
    let values = "break const continue else for if let of return type while";
    for v in values.split_whitespace() {
        result.insert(v);
    }
    result
}

fn symbols() -> HashSet<&'static str> {
    let mut result = HashSet::default();
    let values = "+ += - -= * *= / /= % ** < <= > >= == != === !== =
                  ( ) [ ] { } => ? : . , ; ! ~ | & ^ && || << >> ++ --";
    for v in values.split_whitespace() {
        result.insert(v);
    }
    result
}

fn trie(values: HashSet<&'static str>) -> HashMap<&'static str, bool> {
    let mut result = HashMap::default();
    for v in &values {
        for (offset, ch) in v.char_indices() {
            let prefix = &v[..offset + ch.len_utf8()];
            let ok = prefix.len() == v.len();
            result.entry(prefix).and_modify(|x| *x = *x || ok).or_insert(ok);
        }
    }
    result
}

// Lexer

#[derive(Debug, Eq, PartialEq)]
enum TokenKind {
    Symbol,
    Keyword,
    Identifier,
    DblLiteral,
    IntLiteral,
    StrLiteral,
}

struct Token<'a> {
    kind: TokenKind,
    text: &'a str,
}

fn lex<'a>(input: &'a str, errors: &mut Vec<Error<'a>>) -> Vec<Token<'a>> {
    let mut input = input;
    let mut result = vec![];
    let keywords = trie(keywords());
    let symbols = trie(symbols());

    // All predicates guarantee that the character is ASCII -> len_utf8 == 1.
    let identifier_start = |ch: char| { ch.is_ascii_alphabetic() || ch == '_' };
    let identifier = |ch: char| { identifier_start(ch) || ch.is_ascii_digit() };
    let hex_digit = |ch: char| { ch.is_ascii_hexdigit() };
    let dec_digit = |ch: char| { ch.is_ascii_digit() };
    let oct_digit = |ch: char| { dec_digit(ch) && ch != '8' && ch != '9' };
    let bin_digit = |ch: char| { ch == '0' || ch == '1' };

    let peek = |x: &str| { x.chars().next().unwrap_or('\0') };

    let make_error = |error: String, pos: &'a str| {
        Error { error, pos }
    };

    let make_token = |kind: TokenKind, pos: &'a str, end: &'a str| {
        Token { kind, text: &pos[..pos.len() - end.len()] }
    };

    let match_trie = |input: &'a str, trie: &HashMap<&'static str, bool>| {
        let mut result = None;
        for (offset, ch) in input.char_indices() {
            let prefix = &input[..offset + ch.len_utf8()];
            match trie.get(prefix) {
                Some(true) => { result = Some(prefix); }
                Some(false) => {},
                None => { break; }
            }
        }
        result
    };

    let parse_hex_digits = |input: &mut &'a str, offset: usize, count: usize| {
        let mut current = &input[offset..];
        for _ in 0..count {
            if hex_digit(peek(current)) { return false; }
            current = &current[1..];
        }
        *input = current;
        true
    };

    let parse_char = |input: &mut &'a str, errors: &mut Vec<Error<'a>>, quote: char| {
        let ch = peek(input);
        if ch == '\0' || ch == '\n' || ch == '\r' || ch == quote { return false; }

        let pos = *input;
        *input = &input[ch.len_utf8()..];
        if ch != '\\' { return true; }

        let next = peek(input);
        if next == '\\' || next == '0' || next == '"' || next == '\'' ||
           next == 'b' || next == 'n' || next == 'r' || next == 't' {
            *input = &input[1..];
            return true;
        } else if (next == 'x' && parse_hex_digits(input, 2, 2)) ||
                  (next == 'u' && parse_hex_digits(input, 2, 4)) {
            return true;
        }
        errors.push(make_error("Unknown string escape sequence".into(), pos));
        return true;
    };

    let parse_str = |input: &mut &'a str, errors: &mut Vec<Error<'a>>, quote: char| {
        let pos = *input;
        *input = &input[1..];
        while parse_char(input, errors, quote) {}
        if peek(input) == quote {
            *input = &input[1..];
        } else {
            errors.push(make_error("Unterminated string literal".into(), pos));
        }
        make_token(TokenKind::StrLiteral, pos, input)
    };

    let parse_num = |input: &mut &'a str, ch: char| {
        let pos = *input;
        if ch == '0' {
            let next = peek(&input[1..]);
            if next == 'x' && hex_digit(peek(&input[2..])) {
                *input = &input[3..];
                while hex_digit(peek(input)) { *input = &input[1..]; }
                return make_token(TokenKind::IntLiteral, pos, input);
            } else if next == 'b' && bin_digit(peek(&input[2..])) {
                *input = &input[3..];
                while bin_digit(peek(input)) { *input = &input[1..]; }
                return make_token(TokenKind::IntLiteral, pos, input);
            } else if oct_digit(next) {
                *input = &input[2..];
                while oct_digit(peek(input)) { *input = &input[1..]; }
                return make_token(TokenKind::IntLiteral, pos, input);
            }
        }

        while dec_digit(peek(input)) { *input = &input[1..]; }

        let next = peek(input);
        if next == '.' {
            *input = &input[1..];
            while dec_digit(peek(input)) { *input = &input[1..]; }
            return make_token(TokenKind::DblLiteral, pos, input);
        } else if next == 'e' || next == 'E' {
            if peek(&input[1..]) == '0' {
                *input = &input[2..];
                return make_token(TokenKind::DblLiteral, pos, input);
            }
            *input = &input[1..];
            while dec_digit(peek(input)) { *input = &input[1..]; }
            return make_token(TokenKind::DblLiteral, pos, input);
        }
        make_token(TokenKind::IntLiteral, pos, input)
    };

    let skip_whitespace = |input: &mut &'a str, errors: &mut Vec<Error<'a>>| {
        while !input.is_empty() {
            let ch = peek(input);
            if ch.is_whitespace() {
                *input = &input[ch.len_utf8()..];
            } else if input.starts_with("//") {
                let mut ok = false;
                for (offset, ch) in input[2..].char_indices() {
                    if ch != '\n' { continue; }
                    *input = &input[offset + ch.len_utf8() + 2..];
                    ok = true;
                    break;
                }
                if !ok {
                    *input = &input[input.len()..];
                }
            } else if input.starts_with("/*") {
                let mut ok = false;
                for (offset, _) in input[2..].char_indices() {
                    if !input[offset + 2..].starts_with("*/") { continue; }
                    *input = &input[offset + 4..];
                    ok = true;
                    break;
                }
                if !ok {
                    errors.push(make_error("Unterminated /* comment".into(), input));
                    *input = &input[input.len()..];
                }
            } else {
                return;
            }
        }
    };

    while !input.is_empty() {
        skip_whitespace(&mut input, errors);
        if input.is_empty() { break; }

        let pos = input;
        let ch = peek(input);
        if let Some(x) = match_trie(input, &keywords) &&
           !identifier(peek(&input[x.len()..])) {
            input = &input[x.len()..];
            result.push(make_token(TokenKind::Keyword, pos, input));
        } else if ch == '"' || ch == '\'' {
            result.push(parse_str(&mut input, errors, ch));
        } else if identifier_start(ch) {
            for (offset, ch) in input.char_indices() {
                if !identifier(ch) { break; }
                input = &pos[offset + ch.len_utf8()..];
            }
            result.push(make_token(TokenKind::Identifier, pos, input));
        } else if dec_digit(ch) || (ch == '.' && dec_digit(peek(&input[1..]))) {
            result.push(parse_num(&mut input, ch));
            if identifier(peek(input)) {
                let error = "Numeric literal followed by identifier.".into();
                errors.push(make_error(error, input));
            }
            while identifier(peek(input)) { input = &input[1..]; }
        } else if let Some(x) = match_trie(input, &symbols) {
            input = &input[x.len()..];
            result.push(make_token(TokenKind::Symbol, pos, input));
        } else {
            let error = format!("Unknown symbol: {}", ch);
            errors.push(Error { error, pos: input });
            input = &input[ch.len_utf8()..];
        }
    }
    result
}

// AST

struct Node<'a, T> {
    src: &'a str,
    data: Box<T>,
}

struct Identifier<'a>(Node<'a, ()>);
struct Operator<'a>(Node<'a, ()>);
struct Keyword<'a>(Node<'a, ()>);
struct Expr<'a>(Node<'a, ExprData<'a>>);

enum ExprData<'a> {
    Error,
    Identifier,
    DblLiteral,
    IntLiteral,
    StrLiteral,
    BinaryOp {op: Operator<'a>, lhs: Expr<'a>, rhs: Expr<'a>},
    UnaryPrefixOp {op: Operator<'a>, expr: Expr<'a>},
    UnarySuffixOp {op: Operator<'a>, expr: Expr<'a>},
    Array {elements: Vec<Expr<'a>>},
    Object {items: Vec<(Identifier<'a>, Expr<'a>)>},
    FieldAccess {base: Expr<'a>, field: Identifier<'a>},
    IndexAccess {base: Expr<'a>, index: Expr<'a>},
    Ternary {cond: Expr<'a>, lhs: Expr<'a>, rhs: Expr<'a>},
}

// Parser

struct Env<'a> {
    tokens: Vec<Token<'a>>,
    errors: Vec<Error<'a>>,
    i: usize,
}

fn advance(env: &mut Env) -> bool {
    if env.i == env.tokens.len() { return false; }
    assert!(env.i < env.tokens.len());
    env.i += 1;
    true
}

fn assert_advance(env: &mut Env) {
    let ok = advance(env);
    assert!(ok);
}

fn ahead(env: &Env, i: usize, kind: TokenKind, text: Option<&str>) -> bool {
    if let Some(x) = env.tokens.get(env.i + i) {
        return x.kind == kind && text.unwrap_or(x.text) == x.text;
    }
    false
}

fn cursor<'a>(env: &Env<'a>) -> &'a str {
    env.tokens.get(env.i).map(|x| x.text).unwrap_or("")
}

fn check(env: &Env, kind: TokenKind) -> bool {
    ahead(env, 0, kind, None)
}

fn check_text(env: &Env, kind: TokenKind, text: &str) -> bool {
    ahead(env, 0, kind, Some(text))
}

fn consume(env: &mut Env, kind: TokenKind) -> bool {
    let result = check(env, kind);
    if result { assert_advance(env); }
    result
}

fn consume_text(env: &mut Env, kind: TokenKind, text: &str) -> bool {
    let result = check_text(env, kind, text);
    if result { assert_advance(env); }
    result
}

fn require(env: &mut Env, message: &str, kind: TokenKind) -> bool {
  if consume(env, kind) { return true; }
  env.errors.push(Error { error: message.into(), pos: cursor(env) });
  false
}

fn require_text(env: &mut Env, message: &str, kind: TokenKind, text: &str) -> bool {
  if consume_text(env, kind, text) { return true; }
  env.errors.push(Error { error: message.into(), pos: cursor(env) });
  false
}

fn parse_binop_expr<'a>(env: &mut Env<'a>) -> Expr<'a> {
    unimplemented!()
}

fn parse_expr<'a>(env: &mut Env<'a>) -> Expr<'a> {
    let base = parse_binop_expr(env);
    if !check(env, TokenKind::Symbol) { return base; }

    if consume_text(env, TokenKind::Symbol, "?") {
        let cond = base;
        let lhs = parse_expr(env);
        require_text(env, "Expected: :", TokenKind::Symbol, ":");
        let rhs = parse_expr(env);
        let data = Box::new(ExprData::Ternary { cond, lhs, rhs });
        return Expr(Node { src: "", data });
    }

    base
}

// Entry point

fn format_errors(input: &str, errors: &[Error]) -> Vec<String> {
    let mut line = 0;
    let mut range = (0, 0);
    let mut prev = usize::MAX;
    let mut suffix = input;
    let mut result = vec![];

    let peek = |x: &str| { x.chars().next().unwrap_or('\n') };
    let next = |x: &mut &str| { if !x.is_empty() { *x = &x[peek(x).len_utf8()..] } };

    for error in errors {
        let pos = input.len() - error.pos.len();
        if prev != pos {
            while range.1 <= pos {
                line += 1;
                while peek(suffix) != '\n' { next(&mut suffix); }
                range = (range.1, input.len() - suffix.len() + 1);
                next(&mut suffix);
            }
            prev = pos;
        }
        result.push(format!("{}:{}:{}", line, pos - range.0 + 1, error.error));
    }
    result
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} <filename>", args[0]);
    }
    let input = std::fs::read_to_string(&args[1]).unwrap();
    let mut errors = vec![];
    let tokens = lex(&input, &mut errors);
    let mut env = Env { tokens, errors, i: 0 };
    let _ = parse_expr(&mut env);
    let mut errors = env.errors;
    errors.sort_by_key(|x| -(x.pos.len() as isize));
    for message in format_errors(&input, &errors) {
        println!("{}", message);
    }
}
