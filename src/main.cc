#include "parser.h"

#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <deque>
#include <ios>
#include <iostream>
#include <fstream>
#include <sstream>
#include <system_error>
#include <unordered_map>
#include <vector>

namespace types {

using namespace base;

#define TYPE_CASES(X) \
  X(Dbl)              \
  X(Int)              \
  X(Str)              \
  X(Bool)             \
  X(Null)             \
  X(Void)             \
  X(Error)            \
  X(Array)            \
  X(Tuple)            \
  X(Union)            \
  X(Object)           \
  X(Closure)          \

ENUM(TypeCase, uint8_t, TYPE_CASES);

#undef TYPE_CASES

struct Type {
  protected:
    Type(TypeCase kind_) : kind(kind_) {}

  public:
    virtual ~Type() = default;

    bool accepts(const Type& o) const {
      return error() || o.error() || acceptsInner(o);
    }
    bool matches(const Type& o) const {
      return error() || o.error() || matchesInner(o);
    }

    virtual bool acceptsInner(const Type& o) const = 0;
    virtual bool matchesInner(const Type& o) const = 0;

    virtual std::string show() const = 0;
    virtual std::string showInner() const { return show(); }

    bool error() const { return kind == TypeCase::Error; }
    bool isDbl() const { return kind == TypeCase::Dbl; }
    bool isInt() const { return kind == TypeCase::Int; }
    bool isNum() const { return isInt() || isDbl(); }
    bool isStr() const { return kind == TypeCase::Str; }
    bool isBool() const { return kind == TypeCase::Bool; }
    bool isNull() const { return kind == TypeCase::Null; }
    bool isVoid() const { return kind == TypeCase::Void; }

    const TypeCase kind;
};

#define PRIMITIVE(Case, Name)                                          \
  struct Case##Type final : public Type {                              \
    Case##Type() : Type(TypeCase::Case) {}                             \
    bool acceptsInner(const Type& o) const final {                     \
      return o.kind == TypeCase::Case;                                 \
    }                                                                  \
    bool matchesInner(const Type& o) const final {                     \
      return o.kind == TypeCase::Case;                                 \
    }                                                                  \
    std::string show() const final { return Name; }                    \
  };                                                                   \
  Shared<Type> Get##Case##Type() {                                     \
    static const Shared<Type> result = std::make_shared<Case##Type>(); \
    return result;                                                     \
  }                                                                    \

PRIMITIVE(Int,   "int");
PRIMITIVE(Dbl,   "number");
PRIMITIVE(Str,   "string");
PRIMITIVE(Bool,  "boolean");
PRIMITIVE(Null,  "null");
PRIMITIVE(Void,  "<void>");
PRIMITIVE(Error, "<error>");

#undef PRIMITIVE

const std::unordered_map<std::string, Shared<Type>>& GetPrimitiveTypes() {
  static const std::unordered_map<std::string, Shared<Type>> result = [&]{
    std::unordered_map<std::string, Shared<Type>> init;
    const auto addPrimitive = [&](Shared<Type> type) {
      const auto name = std::string(type->show());
      init[name] = std::move(type);
    };
    addPrimitive(GetDblType());
    addPrimitive(GetIntType());
    addPrimitive(GetStrType());
    addPrimitive(GetBoolType());
    addPrimitive(GetNullType());
    return init;
  }();
  return result;
};

struct ArrayType : public Type {
  ArrayType(Shared<Type> element_)
      : Type(TypeCase::Array), element(std::move(element_)) {}

  bool acceptsInner(const Type& o) const final {
    return matchesInner(o);
  }

  bool matchesInner(const Type& o) const final {
    return o.kind == TypeCase::Array &&
           element->matches(*reinterpret_cast<const ArrayType&>(o).element);
  }

  std::string show() const final {
    std::stringstream ss;
    if (element->kind == TypeCase::Union) ss << '(';
    ss << element->showInner();
    if (element->kind == TypeCase::Union) ss << ')';
    ss << "[]";
    return ss.str();
  }

  Shared<Type> element;
};

struct TupleType : public Type {
  TupleType(std::vector<Shared<Type>> elements_)
      : Type(TypeCase::Tuple), elements(std::move(elements_)) {}

  bool acceptsInner(const Type& o) const final {
    return matchesInner(o);
  }

  bool matchesInner(const Type& o) const final {
    if (o.kind != TypeCase::Tuple) return false;
    const auto& tuple = reinterpret_cast<const TupleType&>(o);
    if (elements.size() != tuple.elements.size()) return false;
    for (size_t i = 0; i < elements.size(); i++) {
      if (!elements[i]->matches(*tuple.elements[i])) return false;
    }
    return true;
  }

  std::string show() const final {
    std::stringstream ss;
    ss << '[';
    for (size_t i = 0; i < elements.size(); i++) {
      if (i > 0) ss << ", ";
      ss << elements[i]->showInner();
    }
    ss << ']';
    return ss.str();
  }

  std::vector<Shared<Type>> elements;
};

struct UnionType : public Type {
  UnionType(std::vector<Shared<Type>> options_)
      : Type(TypeCase::Union), options(std::move(options_)) {}

  // TODO: constrain unions so we can support these options

  bool acceptsInner(const Type& o) const final { return false; }

  bool matchesInner(const Type& o) const final { return false; }

  std::string show() const final {
    std::stringstream ss;
    for (size_t i = 0; i < options.size(); i++) {
      if (i > 0) ss << " | ";
      ss << options[i]->showInner();
    }
    return ss.str();
  }

  std::vector<Shared<Type>> options;
};

struct ObjectType : public Type {
  ObjectType(std::string name) : Type(TypeCase::Object), name(name) {}

  bool addField(std::string name, Shared<Type> type) {
    const auto [_, inserted] = types.insert({name, type});
    if (!inserted) return false;
    fields.push_back(std::move(name));
    return true;
  }

  // Hurray for nominal typing...

  bool acceptsInner(const Type& o) const final { return this == &o; }

  bool matchesInner(const Type& o) const final { return this == &o; }

  std::string showInner() const final { return name; }

  std::string show() const final {
    std::stringstream ss;
    ss << "{";
    for (size_t i = 0; i < fields.size(); i++) {
      if (i > 0) ss << ", ";
      ss << fields[i] << ": ";
      ss << types.at(fields[i])->showInner();
    }
    ss << "}";
    return ss.str();
  }

  const std::string name;
  std::unordered_map<std::string, Shared<Type>> types;
  std::vector<std::string> fields;
};

struct ClosureType : public Type {
  ClosureType(std::vector<std::pair<std::string, Shared<Type>>> a, Shared<Type> r)
      : Type(TypeCase::Closure), args(std::move(a)), result(std::move(r)) {}

  bool acceptsInner(const Type& o) const final { return matchesInner(o); }

  bool matchesInner(const Type& o) const final {
    if (o.kind != TypeCase::Closure) return false;
    const auto& fn = reinterpret_cast<const ClosureType&>(o);
    if (args.size() != fn.args.size()) return false;
    for (size_t i = 0; i < args.size(); i++) {
      if (!args[i].second->matches(*fn.args[i].second)) return false;
    }
    return result->matches(*fn.result);
  }

  std::string show() const final {
    std::stringstream ss;
    ss << "(";
    for (size_t i = 0; i < args.size(); i++) {
      if (i > 0) ss << ", ";
      ss << args[i].first << ": " << args[i].second->show();
    }
    ss << ") => " << result->showInner();
    return ss.str();
  }

  std::vector<std::pair<std::string, Shared<Type>>> args;
  Shared<Type> result;
};

} // namespace types

namespace typecheck {

using namespace ast;
using namespace types;
using parser::Diagnostic;

struct Value {
  bool mut = false;
  Shared<Type> type;
};

struct Scope {
  std::unordered_map<std::string, Shared<Type>> types;
  std::unordered_map<std::string, Value> values;
  Shared<Type> output; // null for non-function blocks
};

struct Env {
  const std::string& input;
  std::vector<Diagnostic>* diagnostics;
  std::deque<Scope> scopes; // we push and pop the front; last scope is global

  // Used to resolve circular references between type aliases.
  std::unordered_map<std::string, const TypeAliasStatementNode&> typeAliases;
  std::vector<std::string> typeAliasStack;
};

size_t cursorHelper(const std::string& input, const Node& node) {
  const auto result = static_cast<size_t>(node.source.data() - input.data());
  const auto bounded = 0 <= result && result < input.size();
  assert(bounded || node.source.empty());
  if (bounded) return result;

  for (const auto& child : node.children) {
    const size_t result = cursorHelper(input, *child);
    if (result != -1) return result;
  }
  return static_cast<size_t>(-1);
}

size_t cursor(Env& env, const Node& node) {
  const size_t result = cursorHelper(env.input, node);
  assert(result != static_cast<size_t>(-1));
  return result;
}

void error(Env& env, const Node& node, const std::string& error) {
  assert(env.diagnostics != nullptr);
  env.diagnostics->push_back({cursor(env, node), error});
}

// Type resolution, including type aliases

bool defineTypeAlias(Env& env, const TypeAliasStatementNode& alias);

Shared<Type> resolveType(Env& env, const TypeNode& type) {
  switch (type.kind) {
    case TypeKind::IdentifierType: {
      if (type.source.empty()) return GetErrorType();
      const std::string name(type.source);
      const auto& primitives = GetPrimitiveTypes();
      if (const auto it = primitives.find(name); it != primitives.end()) {
        return it->second;
      }
      const auto& aliases = env.typeAliases;
      if (const auto it = aliases.find(name); it != aliases.end()) {
        if (!defineTypeAlias(env, it->second)) return GetErrorType();
      }
      for (const auto& scope : env.scopes) {
        const auto it = scope.types.find(name);
        if (it != scope.types.end()) return it->second;
      }
      error(env, type, "Unknown type or type alias");
      return GetErrorType();
    }
    case TypeKind::ArrayType: {
      const auto& sub = reinterpret_cast<const ArrayTypeNode&>(type);
      return std::make_shared<ArrayType>(resolveType(env, sub.element));
    }
    case TypeKind::TupleType: {
      const auto& sub = reinterpret_cast<const TupleTypeNode&>(type);
      std::vector<Shared<Type>> xs;
      for (const auto& x : sub.elements) xs.push_back(resolveType(env, x));
      return std::make_shared<TupleType>(xs);
    }
    case TypeKind::UnionType: {
      const auto& sub = reinterpret_cast<const UnionTypeNode&>(type);
      std::vector<Shared<Type>> xs;
      for (const auto& x : sub.options) xs.push_back(resolveType(env, x));
      return std::make_shared<UnionType>(xs);
    }
    case TypeKind::ObjectType: {
      error(env, type, "Anonymous object type; use a type-alias");
      return GetErrorType();
    }
    case TypeKind::GenericType:
      error(env, type, "Generic types are not yet supported");
      return GetErrorType();
    case TypeKind::ClosureType:
      error(env, type, "Closure types are not yet supported");
      return GetErrorType();
  }
  assert(false);
}

void declareTypeAlias(Env& env, const TypeAliasStatementNode& alias) {
  const auto name = std::string(alias.lhs->source);
  if (GetPrimitiveTypes().count(name) > 0) {
    error(env, *alias.lhs, "Type alias cannot override primitive type");
    return;
  }
  const auto [_, inserted] = env.typeAliases.insert({name, alias});
  if (!inserted) error(env, *alias.lhs, "Duplicate type alias");
}

bool defineTypeAlias(Env& env, const TypeAliasStatementNode& alias) {
  const auto name = std::string(alias.lhs->source);
  if (env.typeAliases.count(name) == 0) return true;

  auto& stack = env.typeAliasStack;
  for (size_t i = 0; i < stack.size(); i++) {
    if (env.typeAliasStack[i] != name) continue;
    std::stringstream ss;
    ss << "Circular type definition: ";
    for (size_t j = i; j < stack.size(); j++) {
      ss << stack[j] << " -> ";
    }
    ss << name;
    error(env, *alias.lhs, ss.str());
    return false;
  }

  auto& types = env.scopes.front().types;
  if (alias.rhs->kind != TypeKind::ObjectType) {
    env.typeAliasStack.push_back(name);
    types[name] = resolveType(env, *alias.rhs);
    env.typeAliasStack.pop_back();
    env.typeAliases.erase(name);
    return false;
  }

  decltype(env.typeAliasStack) tmp;
  std::swap(tmp, env.typeAliasStack);
  auto result = std::make_shared<ObjectType>(name);
  env.typeAliases.erase(name);
  types[name] = result;

  const auto& sub = reinterpret_cast<const ObjectTypeNode&>(*alias.rhs);
  for (const NameTypePairNode& item : sub.items) {
    auto name = std::string(item.name.source);
    auto type = resolveType(env, item.type);
    const auto ok = result->addField(std::move(name), std::move(type));
    if (!ok) error(env, item.name, "Duplicate object key");
  }

  std::swap(tmp, env.typeAliasStack);
  return true;
}

// Type-checking expressions

Shared<Type> typecheckExpr(Env& env, ExprNode& expr);

void typecheckBlock(Env& env, const Refs<StatementNode>& block);

Shared<Type> typecheckExprInner(Env& env, ExprNode& expr) {
  switch (expr.kind) {
    // Trivial cases:
    case ExprKind::ErrorExpr:       return GetErrorType();
    case ExprKind::DblLiteralExpr:  return GetDblType();
    case ExprKind::IntLiteralExpr:  return GetIntType();
    case ExprKind::StrLiteralExpr:  return GetStrType();
    case ExprKind::BoolLiteralExpr: return GetBoolType();
    case ExprKind::NullLiteralExpr: return GetNullType();
    // Non-trivial cases:
    case ExprKind::BinaryOpExpr: {
      const auto& sub = reinterpret_cast<const BinaryOpExprNode&>(expr);
      auto lhs = typecheckExpr(env, sub.lhs);
      auto rhs = typecheckExpr(env, sub.rhs);
      if (lhs->error() && rhs->error()) return GetErrorType();
      if (lhs->error()) lhs = rhs;
      if (rhs->error()) rhs = lhs;

      const auto op = sub.op.source;
      if (op == "*" || op == "/" || op == "+" || op == "-") {
        if (lhs->isInt() && rhs->isInt()) return GetIntType();
        if (lhs->isNum() && rhs->isNum()) return GetDblType();
        if (op == "+" && lhs->isStr() && rhs->isStr()) return GetStrType();
      } else if (op == "%" || op == "<<" || op == ">>") {
        if (lhs->isInt() && rhs->isInt()) return GetIntType();
      } else if (op == "<" || op == "<=" || op == ">" || op == ">=") {
        if (lhs->isNum() && rhs->isNum()) return GetBoolType();
        if (lhs->isStr() && rhs->isStr()) return GetBoolType();
      }

      std::stringstream ss;
      ss << "Binary op " << op << " got invalid types: ";
      ss << lhs->show() << " and " << rhs->show();
      error(env, sub.op, ss.str());
      return GetErrorType();
    }
    case ExprKind::UnaryPrefixOpExpr: {
      const auto& sub = reinterpret_cast<const UnaryPrefixOpExprNode&>(expr);
      error(env, sub, "UnaryPrefixOpExpr is not yet supported");
      return GetErrorType();
    }
    case ExprKind::UnarySuffixOpExpr: {
      const auto& sub = reinterpret_cast<const UnarySuffixOpExprNode&>(expr);
      error(env, sub, "UnarySuffixOpExpr is not yet supported");
      return GetErrorType();
    }
    case ExprKind::ArrayExpr: {
      const auto& sub = reinterpret_cast<const ArrayExprNode&>(expr);
      error(env, sub, "ArrayExpr is not yet supported");
      return GetErrorType();
    }
    case ExprKind::ObjectExpr: {
      const auto& sub = reinterpret_cast<const ObjectExprNode&>(expr);
      error(env, sub, "ObjectExpr is not yet supported");
      return GetErrorType();
    }
    case ExprKind::ClosureExpr: {
      const auto& sub = reinterpret_cast<const ClosureExprNode&>(expr);
      env.scopes.push_front({});

      auto& scope = env.scopes.front();
      std::vector<std::pair<std::string, Shared<Type>>> args;
      for (const auto& arg : sub.args) {
        const auto node = arg.get().type;
        if (!node) error(env, arg, "Arg type inference is not yet supported");
        auto name = std::string(arg.get().name.source);
        auto type = node ? resolveType(env, *node) : GetErrorType();
        args.push_back({name, type});
        const auto [_, inserted] = scope.values.insert(
              {std::move(name), {.mut = true, .type = std::move(type)}});
        if (inserted) continue;
        std::stringstream ss;
        ss << "Duplicate arg name: " << name;
        error(env, arg.get().name, ss.str());
      }
      auto result = sub.result ? resolveType(env, *sub.result) : GetVoidType();
      scope.output = result;

      typecheckBlock(env, sub.body.statements);
      env.scopes.pop_front();
      return std::make_shared<ClosureType>(std::move(args), result);
    }
    case ExprKind::TernaryExpr: {
      const auto& sub = reinterpret_cast<const TernaryExprNode&>(expr);
      error(env, sub, "TernaryExpr is not yet supported");
      return GetErrorType();
    }
    case ExprKind::TemplateExpr: {
      const auto& sub = reinterpret_cast<const TemplateExprNode&>(expr);
      error(env, sub, "TemplateExpr is not yet supported");
      return GetErrorType();
    }
    case ExprKind::AssignmentExpr: {
      const auto& sub = reinterpret_cast<const AssignmentExprNode&>(expr);
      if (sub.lhs.kind != ExprKind::IdentifierExpr) {
        error(env, sub, "Destructuring assignments are not yet supported");
      }
      error(env, sub, "AssignmentExpr is not yet supported");
      return GetErrorType();
    }
    case ExprKind::IdentifierExpr: {
      const auto& sub = reinterpret_cast<const IdentifierExprNode&>(expr);
      const auto name = std::string(sub.source);
      for (const auto& scope : env.scopes) {
        const auto it = scope.values.find(name);
        if (it != scope.values.end()) return it->second.type;
      }

      std::stringstream ss;
      ss << "Unknown local: " << name;
      error(env, sub, ss.str());
      return GetErrorType();
    }
    case ExprKind::FieldAccessExpr: {
      const auto& sub = reinterpret_cast<const FieldAccessExprNode&>(expr);
      error(env, sub, "FieldAccessExpr is not yet supported");
      return GetErrorType();
    }
    case ExprKind::IndexAccessExpr: {
      const auto& sub = reinterpret_cast<const IndexAccessExprNode&>(expr);
      error(env, sub, "IndexAccessExpr is not yet supported");
      return GetErrorType();
    }
    case ExprKind::FunctionCallExpr: {
      const auto& sub = reinterpret_cast<const FunctionCallExprNode&>(expr);
      error(env, sub, "FunctionCallExpr is not yet supported");
      return GetErrorType();
    }
    case ExprKind::ConstructorCallExpr: {
      const auto& sub = reinterpret_cast<const ConstructorCallExprNode&>(expr);
      error(env, sub, "ConstructorCallExpr is not yet supported");
      return GetErrorType();
    }
  }
  return GetErrorType();
}

Shared<Type> typecheckExpr(Env& env, ExprNode& expr) {
  assert(expr.type == nullptr);
  expr.type = typecheckExprInner(env, expr);
  return expr.type;
}

// Type-checking statements

void typecheckStatement(Env& env, StatementNode& statement) {
  switch (statement.kind) {
    case StatementKind::IfStatement: {
      const auto& sub = reinterpret_cast<const IfStatementNode&>(statement);
      for (const auto& cond : sub.cases) {
        typecheckExpr(env, cond.get().cond);
        typecheckStatement(env, cond.get().then);
      }
      if (sub.elseCase) typecheckStatement(env, *sub.elseCase);
      return;
    }
    case StatementKind::ExprStatement: {
      const auto& sub = reinterpret_cast<const ExprStatementNode&>(statement);
      typecheckExpr(env, sub.expr);
      return;
    }
    case StatementKind::BlockStatement: {
      const auto& sub = reinterpret_cast<const BlockStatementNode&>(statement);
      env.scopes.push_front({});
      typecheckBlock(env, sub.statements);
      env.scopes.pop_front();
      return;
    }
    case StatementKind::DeclarationStatement: {
      const auto& sub = reinterpret_cast<const DeclarationStatementNode&>(statement);
      assert(sub.keyword.source == "const" || sub.keyword.source == "let");
      const auto mut = sub.keyword.source == "let";
      if (sub.root.kind != ExprKind::IdentifierExpr) {
        error(env, sub, "Destructuring declarations are not yet supported");
      }
      auto name = std::string(sub.root.source);
      auto type = typecheckExpr(env, sub.expr);
      auto& scope = env.scopes.front();
      const auto [_, inserted] = scope.values.insert(
          {std::move(name), {.mut = mut, .type = std::move(type)}});
      if (inserted) return;
      std::stringstream ss;
      ss << "Duplicate identifier: " << name;
      error(env, sub, ss.str());
      return;
    }
    case StatementKind::ReturnStatement: {
      const auto& sub = reinterpret_cast<const ReturnStatementNode&>(statement);
      const auto type = sub.expr ? typecheckExpr(env, *sub.expr) : GetVoidType();
      auto expected = [&]() -> const Type* {
        for (const auto& scope : env.scopes) {
          if (scope.output) return scope.output.get();
        }
        return nullptr;
      }();
      if (!expected) {
        error(env, sub, "Unexpected return outside of a function");
      } else if (!expected->accepts(*type)) {
        std::stringstream ss;
        ss << "Invalid return type. Expected: " << expected->show();
        ss << "; got: " << type->show();
        error(env, sub, ss.str());
      }
      return;
    }
    case StatementKind::ClassDeclarationStatement: {
      error(env, statement, "Class declarations are not yet supported");
      return;
    }
    case StatementKind::BreakStatement:
    case StatementKind::ContinueStatement:
    case StatementKind::ForEachStatement:
    case StatementKind::ForLoopStatement:
    case StatementKind::WhileLoopStatement: {
      error(env, statement, "Loop constructs are not yet supported");
      return;
    }
    case StatementKind::EmptyStatement:
    case StatementKind::TypeAliasStatement: {
      return;
    }
  }
  assert(false);
}

void typecheckBlock(Env& env, const Refs<StatementNode>& block) {
  const auto depth = env.scopes.size() - 1;
  const auto pad = std::string(2 * depth, ' ');
  std::cerr << pad << "Scope (depth: " << depth << "): begin" << std::endl;

  assert(env.typeAliases.empty());
  assert(env.typeAliasStack.empty());
  for (auto& statement : block) {
    if (statement.get().kind != StatementKind::TypeAliasStatement) continue;
    declareTypeAlias(env, reinterpret_cast<TypeAliasStatementNode&>(statement.get()));
  }
  for (auto& statement : block) {
    if (statement.get().kind != StatementKind::TypeAliasStatement) continue;
    defineTypeAlias(env, reinterpret_cast<TypeAliasStatementNode&>(statement.get()));
  }
  assert(env.typeAliases.empty());
  assert(env.typeAliasStack.empty());
  for (auto& statement : block) {
    if (statement.get().kind == StatementKind::TypeAliasStatement) continue;
    typecheckStatement(env, statement.get());
  }
  std::cerr << pad << "Scope (depth: " << depth << "): end" << std::endl;
  for (const auto& [name, type] : env.scopes.front().types) {
    std::cerr << pad << "Type: " << name << " => " << type->show() << std::endl;
  }
  for (const auto& [name, value] : env.scopes.front().values) {
    std::cerr << pad << "Value: " << (value.mut ? "let " : "const ") << name;
    std::cerr << " => " << value.type->show() << std::endl;
  }
}

void typecheck(const std::string& input,
               ProgramNode& program,
               std::vector<Diagnostic>* diagnostics) {
  Env env{input, diagnostics};
  env.scopes.push_front({});
  typecheckBlock(env, program.statements);
  env.scopes.pop_front();
  assert(env.scopes.empty());
}

} // namespace typecheck

int main(int argc, const char** argv) {
  if (!(argc == 2 || (argc == 3 && std::string(argv[2]) == "-v"))) {
    std::cerr << "Usage: " << argv[0] << " $FILE [-v]" << std::endl;
    return 1;
  }

  std::ifstream is(argv[1]);
  std::stringstream ss;
  while (is >> ss.rdbuf());
  const auto input = ss.str();
  const bool verbose = argc == 3;

  std::vector<parser::Diagnostic> diagnostics;
  const auto program = parser::parse(input, &diagnostics);
  if (verbose) std::cerr << parser::formatAST(*program) << std::endl;
  typecheck::typecheck(input, *program, &diagnostics);
  std::cerr << std::endl;
  std::cerr << parser::formatDiagnostics(input, &diagnostics);
  return diagnostics.empty() ? 0 : 1;
}
