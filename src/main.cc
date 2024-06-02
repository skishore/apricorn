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
  X(Error)            \
  X(Array)            \
  X(Tuple)            \
  X(Union)            \
  X(Object)           \

ENUM(TypeCase, uint8_t, TYPE_CASES);

#undef TYPE_CASES

struct Type {
  protected:
    Type(TypeCase kind_) : kind(kind_) {}

  public:
    virtual ~Type() = default;
    virtual std::string show() const = 0;
    virtual std::string showInner() const { return show(); }

    const TypeCase kind;
};

#define PRIMITIVE(Case, Name)                       \
  struct Case##Type final : public Type {           \
    Case##Type() : Type(TypeCase::Case) {}          \
    std::string show() const final { return Name; } \
  };                                                \

PRIMITIVE(Int,   "int");
PRIMITIVE(Dbl,   "number");
PRIMITIVE(Str,   "string");
PRIMITIVE(Bool,  "boolean");
PRIMITIVE(Null,  "null");
PRIMITIVE(Error, "<error>");

#undef PRIMITIVE

Shared<Type> GetErrorType() {
  static const Shared<Type> result = std::make_shared<ErrorType>();
  return result;
}

const std::unordered_map<std::string, Shared<Type>>& GetPrimitiveTypes() {
  static const std::unordered_map<std::string, Shared<Type>> result = [&]{
    std::unordered_map<std::string, Shared<Type>> init;
    const auto addPrimitive = [&](Shared<Type> type) {
      const std::string name = type->show();
      init[name] = std::move(type);
    };
    addPrimitive(std::make_shared<DblType>());
    addPrimitive(std::make_shared<IntType>());
    addPrimitive(std::make_shared<StrType>());
    addPrimitive(std::make_shared<BoolType>());
    addPrimitive(std::make_shared<NullType>());
    return init;
  }();
  return result;
};

struct ArrayType : public Type {
  ArrayType(Shared<Type> element_)
      : Type(TypeCase::Array), element(std::move(element_)) {}

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

} // namespace types

namespace typecheck {

using namespace ast;
using namespace types;
using parser::Diagnostic;

struct Scope {
  std::unordered_map<std::string, Shared<Type>> types;
  std::unordered_map<std::string, Shared<Type>> values;
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
  if (!node.source.empty()) {
    const size_t result = static_cast<size_t>(node.source.data() - input.data());
    assert(0 <= result && result < input.size());
    return result;
  }
  for (const auto& child : node.children) {
    const size_t result = cursorHelper(input, *child);
    if (result == -1) return result;
  }
  return static_cast<size_t>(-1);
}

size_t cursor(Env* env, const Node& node) {
  const size_t result = cursorHelper(env->input, node);
  assert(result != static_cast<size_t>(-1));
  return result;
}

void error(Env* env, const Node& node, const std::string& error) {
  assert(env->diagnostics != nullptr);
  env->diagnostics->push_back({cursor(env, node), error});
}

bool defineTypeAlias(Env* env, const TypeAliasStatementNode& alias);

Shared<Type> resolveType(Env* env, const TypeNode& type) {
  switch (type.kind) {
    case TypeKind::IdentifierType: {
      assert(!type.source.empty());
      const std::string name(type.source);
      const auto& primitives = GetPrimitiveTypes();
      if (const auto it = primitives.find(name); it != primitives.end()) {
        return it->second;
      }
      const auto& aliases = env->typeAliases;
      if (const auto it = aliases.find(name); it != aliases.end()) {
        if (!defineTypeAlias(env, it->second)) return GetErrorType();
      }
      for (const Scope& scope : env->scopes) {
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

void declareTypeAlias(Env* env, const TypeAliasStatementNode& alias) {
  std::string name(alias.lhs->source);
  if (GetPrimitiveTypes().count(name) > 0) {
    error(env, *alias.lhs, "Type alias cannot override primitive type");
    return;
  }
  const auto [_, inserted] = env->typeAliases.insert({name, alias});
  if (!inserted) error(env, *alias.lhs, "Duplicate type alias");
}

bool defineTypeAlias(Env* env, const TypeAliasStatementNode& alias) {
  std::string name(alias.lhs->source);
  if (env->typeAliases.count(name) == 0) return true;

  auto& stack = env->typeAliasStack;
  for (size_t i = 0; i < stack.size(); i++) {
    if (env->typeAliasStack[i] != name) continue;
    std::stringstream ss;
    ss << "Circular type definition: ";
    for (size_t j = i; j < stack.size(); j++) {
      ss << stack[j] << " -> ";
    }
    ss << name;
    error(env, *alias.lhs, ss.str());
    return false;
  }

  auto& types = env->scopes.front().types;
  if (alias.rhs->kind != TypeKind::ObjectType) {
    env->typeAliasStack.push_back(name);
    types[name] = resolveType(env, *alias.rhs);
    env->typeAliasStack.pop_back();
    env->typeAliases.erase(name);
    return false;
  }

  decltype(env->typeAliasStack) tmp;
  std::swap(tmp, env->typeAliasStack);
  auto result = std::make_shared<ObjectType>(name);
  env->typeAliases.erase(name);
  types[name] = result;

  const auto& sub = reinterpret_cast<const ObjectTypeNode&>(*alias.rhs);
  for (const NameTypePairNode& item : sub.items) {
    auto name = std::string(item.name.source);
    auto type = resolveType(env, item.type);
    const auto ok = result->addField(std::move(name), std::move(type));
    if (!ok) error(env, item.name, "Duplicate object key");
  }

  std::swap(tmp, env->typeAliasStack);
  return true;
}

void typecheck(Env* env, Refs<StatementNode>& block);

void typecheck(StatementNode& statement) {}

void typecheck(Env* env, Refs<StatementNode>& block) {
  env->scopes.push_front({});
  assert(env->typeAliases.empty());
  assert(env->typeAliasStack.empty());
  for (auto& statement : block) {
    if (statement.get().kind != StatementKind::TypeAliasStatement) continue;
    declareTypeAlias(env, reinterpret_cast<TypeAliasStatementNode&>(statement.get()));
  }
  for (auto& statement : block) {
    if (statement.get().kind != StatementKind::TypeAliasStatement) continue;
    defineTypeAlias(env, reinterpret_cast<TypeAliasStatementNode&>(statement.get()));
  }
  assert(env->typeAliases.empty());
  assert(env->typeAliasStack.empty());
  for (auto& statement : block) {
    if (statement.get().kind == StatementKind::TypeAliasStatement) continue;
    typecheck(statement.get());
  }
  for (const auto& pair : env->scopes.front().types) {
    std::cerr << pair.first << " => " << pair.second->show() << std::endl;
  }
  env->scopes.pop_front();
}

void typecheck(const std::string& input,
               ProgramNode& program,
               std::vector<Diagnostic>* diagnostics) {
  Env env{input, diagnostics};
  typecheck(&env, program.statements);
}

} // namespace typecheck

int main(int argc, const char** argv) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " $FILE" << std::endl;
    return 1;
  }

  std::ifstream is(argv[1]);
  std::stringstream ss;
  while (is >> ss.rdbuf());
  const auto input = ss.str();

  std::vector<parser::Diagnostic> diagnostics;
  const auto program = parser::parse(input, &diagnostics);
  typecheck::typecheck(input, *program, &diagnostics);
  //std::cerr << parser::formatAST(*program);
  //std::cerr << std::endl;
  std::cerr << parser::formatDiagnostics(input, &diagnostics);
  return diagnostics.empty() ? 0 : 1;
}
