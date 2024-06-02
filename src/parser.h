#pragma once

#include <memory>
#include <string>
#include <string_view>

namespace base {

#define ENUM_HELPER_NAME(x) x,
#define ENUM_HELPER_CASE(x) case Impl::x: return #x;
#define ENUM(Name, Type, Options)   \
  enum class Name : Type {          \
    Options(ENUM_HELPER_NAME)       \
  };                                \
  namespace Name##Detail {          \
  enum class Impl : Type {          \
    Options(ENUM_HELPER_NAME)       \
  };                                \
  } /* namespace Name##Detail */    \
  inline const char* desc(Name x) { \
    using Name##Detail::Impl;       \
    switch (static_cast<Impl>(x)) { \
      Options(ENUM_HELPER_CASE)     \
    }                               \
  };                                \

template <typename T> using Ptr  = std::unique_ptr<T>;
template <typename T> using Ptrs = std::vector<Ptr<T>>;
template <typename T> using Refs = std::vector<std::reference_wrapper<T>>;
template <typename T> using Shared = std::shared_ptr<T>;

} // namespace base

namespace ast {

using namespace base;

#define TYPE_KINDS(X) \
  X(IdentifierType)   \
  X(ArrayType)        \
  X(TupleType)        \
  X(UnionType)        \
  X(ObjectType)       \
  X(GenericType)      \
  X(ClosureType)      \

ENUM(TypeKind, uint8_t, TYPE_KINDS);

#undef TYPE_KINDS

#define EXPR_KINDS(X)    \
  X(ErrorExpr)           \
  X(BinaryOpExpr)        \
  X(UnaryPrefixOpExpr)   \
  X(UnarySuffixOpExpr)   \
  X(ArrayExpr)           \
  X(ObjectExpr)          \
  X(ClosureExpr)         \
  X(TernaryExpr)         \
  X(TemplateExpr)        \
  X(AssignmentExpr)      \
  X(IdentifierExpr)      \
  X(DblLiteralExpr)      \
  X(IntLiteralExpr)      \
  X(StrLiteralExpr)      \
  X(FieldAccessExpr)     \
  X(IndexAccessExpr)     \
  X(FunctionCallExpr)    \
  X(ConstructorCallExpr) \

ENUM(ExprKind, uint8_t, EXPR_KINDS);

#undef EXPR_KINDS

#define STATEMENT_KINDS(X)     \
  X(IfStatement)               \
  X(ExprStatement)             \
  X(EmptyStatement)            \
  X(BlockStatement)            \
  X(ForEachStatement)          \
  X(ForLoopStatement)          \
  X(WhileLoopStatement)        \
  X(ReturnStatement)           \
  X(BreakStatement)            \
  X(ContinueStatement)         \
  X(DeclarationStatement)      \
  X(ClassDeclarationStatement) \
  X(TypeAliasStatement)        \

ENUM(StatementKind, uint8_t, STATEMENT_KINDS);

#undef STATEMENT_KINDS

struct Node {
  protected:
    Node() {}
    Node(const Node& o) = delete;
    Node& operator=(const Node& o) = delete;
    Node(Node&& o) = default;
    Node& operator=(Node&& o) = default;

  public:
    virtual ~Node() = default;
    virtual const char* describe() const = 0;

    Ptrs<Node> children;
    std::string_view source;
};

#define BASIC_NODE(name)                                       \
  struct name##Node : public Node {                            \
    name##Node(std::string_view source_) { source = source_; } \
    const char* describe() const final { return #name; }       \
  };

BASIC_NODE(Identifier)
BASIC_NODE(Keyword)
BASIC_NODE(Operator)
BASIC_NODE(Template)

#undef BASIC_NODE

struct BlockStatementNode;
struct StatementNode;
struct ExprNode;
struct TypeNode;

struct ArgDefinitionNode : public Node {
  ArgDefinitionNode(const IdentifierNode& n, const TypeNode* t) : name(n), type(t) {}

  const char* describe() const final { return "ArgDefinition"; }

  const IdentifierNode& name;
  const TypeNode* const type;
};

struct CallArgsNode : public Node {
  CallArgsNode(Refs<ExprNode>&& a) : args(std::move(a)) {}

  const char* describe() const final { return "CallArgs"; }

  Refs<ExprNode> const args;
};

struct CondClauseNode : public Node {
  CondClauseNode(const ExprNode& c, const StatementNode& s) : cond(c), then(s) {}

  const char* describe() const final { return "CondClause"; }

  const ExprNode& cond;
  const StatementNode& then;
};

struct NameTypePairNode : public Node {
  NameTypePairNode(const char* d, const IdentifierNode& n, const TypeNode& t)
      : description(d), name(n), type(t) {}

  const char* describe() const final { return description; }

  const char* const description;
  const IdentifierNode& name;
  const TypeNode& type;
};

struct ObjectItemNode : public Node {
  ObjectItemNode(const IdentifierNode& n, const ExprNode& e) : name(n), expr(e) {}

  const char* describe() const final { return "ObjectItem"; }

  const IdentifierNode& name;
  const ExprNode& expr;
};

struct ClassMemberNode : public Node {
  ClassMemberNode(const KeywordNode* a, const IdentifierNode& n,
                  const TypeNode* t, const ExprNode* i)
      : access(a), name(n), type(t), init(i) {}

  const char* describe() const final { return "ClassMember"; }

  const KeywordNode* const access;
  const IdentifierNode& name;
  const TypeNode* const type;
  const ExprNode* const init;
};

struct ClassMethodNode : public Node {
  ClassMethodNode(const KeywordNode* a, const IdentifierNode& n,
                  Refs<ArgDefinitionNode>&& g, const TypeNode* r,
                  const BlockStatementNode* b, const ExprNode* e)
      : access(a), name(n), result(r), blockBody(b), exprBody(e) {}

  const char* describe() const final { return "ClassMethod"; }

  const KeywordNode* const access;
  const IdentifierNode& name;
  Refs<ArgDefinitionNode> const args;
  const TypeNode* const result;
  const BlockStatementNode* const blockBody;
  const ExprNode* const exprBody;
};

struct TypeLHSNode : public Node {
  TypeLHSNode(const IdentifierNode& b, Refs<IdentifierNode> g)
      : base(b), generics(g) {}

  const char* describe() const final { return "TypeLHS"; }

  const IdentifierNode& base;
  Refs<IdentifierNode> generics;
};

// Type

struct TypeNode : public Node {
  protected:
    TypeNode(TypeKind kind_) : kind(kind_) {}

  public:
    const char* describe() const final { return desc(kind); }

    const TypeKind kind;
};

struct IdentifierTypeNode : public TypeNode {
  IdentifierTypeNode(std::string_view source_)
      : TypeNode(TypeKind::IdentifierType) { source = source_; }
};

struct ArrayTypeNode : public TypeNode {
  ArrayTypeNode(const TypeNode& e)
      : TypeNode(TypeKind::ArrayType), element(e) {}

  const TypeNode& element;
};

struct TupleTypeNode : public TypeNode {
  TupleTypeNode(Refs<TypeNode>&& e)
      : TypeNode(TypeKind::TupleType), elements(e) {}

  Refs<TypeNode> const elements;
};

struct UnionTypeNode : public TypeNode {
  UnionTypeNode(Refs<TypeNode>&& o)
      : TypeNode(TypeKind::UnionType), options(std::move(o)) {}

  Refs<TypeNode> const options;
};

struct ObjectTypeNode : public TypeNode {
  ObjectTypeNode(Refs<NameTypePairNode>&& i)
      : TypeNode(TypeKind::ObjectType), items(std::move(i)) {}

  Refs<NameTypePairNode> const items;
};

struct GenericTypeNode : public TypeNode {
  GenericTypeNode(const IdentifierNode& b, Refs<TypeNode>&& g)
      : TypeNode(TypeKind::GenericType), base(b), generics(std::move(g)) {}

  const IdentifierNode& base;
  Refs<TypeNode> const generics;
};

struct ClosureTypeNode : public TypeNode {
  ClosureTypeNode(Refs<NameTypePairNode>&& a, const TypeNode& r)
      : TypeNode(TypeKind::ClosureType), args(std::move(a)), result(r) {}

  Refs<NameTypePairNode> args;
  const TypeNode& result;
};

// Expr

struct ExprNode : public Node {
  protected:
    ExprNode(ExprKind kind_) : kind(kind_) {}

  public:
    const char* describe() const final { return desc(kind); }

    const ExprKind kind;
};

struct ErrorExprNode : public ExprNode {
  ErrorExprNode() : ExprNode(ExprKind::ErrorExpr) {}
};

struct BinaryOpExprNode : public ExprNode {
  BinaryOpExprNode(const OperatorNode& o, const ExprNode& l, const ExprNode& r)
      : ExprNode(ExprKind::BinaryOpExpr), op(o), lhs(l), rhs(r) {}

  const OperatorNode& op;
  const ExprNode& lhs;
  const ExprNode& rhs;
};

struct UnaryPrefixOpExprNode : public ExprNode {
  UnaryPrefixOpExprNode(const OperatorNode& o, const ExprNode& e)
      : ExprNode(ExprKind::UnaryPrefixOpExpr), op(o), expr(e) {}

  const OperatorNode& op;
  const ExprNode& expr;
};

struct UnarySuffixOpExprNode : public ExprNode {
  UnarySuffixOpExprNode(const OperatorNode& o, const ExprNode& e)
      : ExprNode(ExprKind::UnarySuffixOpExpr), op(o), expr(e) {}

  const OperatorNode& op;
  const ExprNode& expr;
};

struct ArrayExprNode : public ExprNode {
  ArrayExprNode(Refs<ExprNode>&& e)
      : ExprNode(ExprKind::ArrayExpr), elements(std::move(e)) {}

  Refs<ExprNode> const elements;
};

struct ObjectExprNode : public ExprNode {
  ObjectExprNode(Refs<ObjectItemNode>&& i)
      : ExprNode(ExprKind::ObjectExpr), items(i) {}

  Refs<ObjectItemNode> const items;
};

struct ClosureExprNode : public ExprNode {
  ClosureExprNode(Refs<ArgDefinitionNode>&& a, const TypeNode* t,
                  const BlockStatementNode* b, const ExprNode* e)
      : ExprNode(ExprKind::ClosureExpr), args(std::move(a)),
        result(t), blockBody(b), exprBody(e) {}

  Refs<ArgDefinitionNode> const args;
  const TypeNode* const result;
  const BlockStatementNode* const blockBody;
  const ExprNode* const exprBody;
};

struct TernaryExprNode : public ExprNode {
  TernaryExprNode(const ExprNode& c, const ExprNode& l, const ExprNode& r)
      : ExprNode(ExprKind::TernaryExpr), cond(c), lhs(l), rhs(r) {}

  const ExprNode& cond;
  const ExprNode& lhs;
  const ExprNode& rhs;
};

struct TemplateExprNode : public ExprNode {
  struct TemplatePair {
    const ExprNode& expr;
    const TemplateNode& text;
  };

  TemplateExprNode(const TemplateNode& p, std::vector<TemplatePair>&& s)
      : ExprNode(ExprKind::TemplateExpr), prefix(p), suffixes(std::move(s)) {}

  const TemplateNode& prefix;
  std::vector<TemplatePair> suffixes;
};

struct AssignmentExprNode : public ExprNode {
  AssignmentExprNode(const OperatorNode& o, const ExprNode& l, const ExprNode& r)
      : ExprNode(ExprKind::AssignmentExpr), op(o), lhs(l), rhs(r) {}

  const OperatorNode& op;
  const ExprNode& lhs; // TODO: new type for LHS "exprs"
  const ExprNode& rhs;
};

struct IdentifierExprNode : public ExprNode {
  IdentifierExprNode(std::string_view source_)
      : ExprNode(ExprKind::IdentifierExpr) { source = source_; }
};

struct DblLiteralExprNode : public ExprNode {
  DblLiteralExprNode(std::string_view source_)
      : ExprNode(ExprKind::DblLiteralExpr) { source = source_; }
};

struct IntLiteralExprNode : public ExprNode {
  IntLiteralExprNode(std::string_view source_)
      : ExprNode(ExprKind::IntLiteralExpr) { source = source_; }
};

struct StrLiteralExprNode : public ExprNode {
  StrLiteralExprNode(std::string_view source_)
      : ExprNode(ExprKind::StrLiteralExpr) { source = source_; }
};

struct FieldAccessExprNode : public ExprNode {
  FieldAccessExprNode(const ExprNode& b, const IdentifierNode& f)
      : ExprNode(ExprKind::FieldAccessExpr), base(b), field(f) {}

  const ExprNode& base;
  const IdentifierNode& field;
};

struct IndexAccessExprNode : public ExprNode {
  IndexAccessExprNode(const ExprNode& b, const ExprNode& i)
      : ExprNode(ExprKind::IndexAccessExpr), base(b), index(i) {}

  const ExprNode& base;
  const ExprNode& index;
};

struct FunctionCallExprNode : public ExprNode {
  FunctionCallExprNode(const ExprNode& f, const CallArgsNode& a)
      : ExprNode(ExprKind::FunctionCallExpr), fn(f), args(a) {}

  const ExprNode& fn;
  const CallArgsNode& args;
};

struct ConstructorCallExprNode : public ExprNode {
  ConstructorCallExprNode(const IdentifierNode& c, const CallArgsNode& a)
      : ExprNode(ExprKind::ConstructorCallExpr), cls(c), args(a) {}

  const IdentifierNode& cls;
  const CallArgsNode& args;
};

// Statement

struct StatementNode : public Node {
  StatementNode() : kind(StatementKind::TypeAliasStatement) {}

  protected:
    StatementNode(StatementKind kind_) : kind(kind_) {}

  public:
    const char* describe() const final { return desc(kind); }

    const StatementKind kind;
};

struct IfStatementNode : public StatementNode {
  IfStatementNode(Refs<CondClauseNode>&& c, const StatementNode* e)
      : StatementNode(StatementKind::IfStatement), cases(std::move(c)), elseCase(e) {}

  Refs<CondClauseNode> const cases;
  const StatementNode* const elseCase;
};

struct ExprStatementNode : public StatementNode {
  ExprStatementNode(const ExprNode& e)
      : StatementNode(StatementKind::ExprStatement), expr(e) {}

  const ExprNode& expr;
};

struct EmptyStatementNode : public StatementNode {
  EmptyStatementNode() : StatementNode(StatementKind::EmptyStatement) {}
};

struct BlockStatementNode : public StatementNode {
  BlockStatementNode(Refs<StatementNode>&& s)
      : StatementNode(StatementKind::BlockStatement), statements(std::move(s)) {}

  Refs<StatementNode> const statements;
};

struct DeclarationStatementNode : public StatementNode {
  DeclarationStatementNode(const KeywordNode& k, const ExprNode& r,
                           const TypeNode* t, const ExprNode& e)
      : StatementNode(StatementKind::DeclarationStatement),
        keyword(k), root(r), type(t), expr(e) {}

  const KeywordNode& keyword;
  const ExprNode& root;
  const TypeNode* const type;
  const ExprNode& expr;
};

struct ClassDeclarationStatementNode : public StatementNode {
  ClassDeclarationStatementNode(const IdentifierNode& n, const IdentifierNode* b,
                                Refs<ClassMemberNode>&& m, Refs<ClassMethodNode>&& f)
      : StatementNode(StatementKind::ClassDeclarationStatement),
        name(n), base(b), members(std::move(m)), methods(std::move(f)) {}

  const IdentifierNode& name;
  const IdentifierNode* const base;
  Refs<ClassMemberNode> members;
  Refs<ClassMethodNode> methods;
};

struct ForEachStatementNode : public StatementNode {
  ForEachStatementNode() : StatementNode(StatementKind::ForEachStatement) {}

  struct Initializer {
    const KeywordNode* keyword;
    const ExprNode* root;
    const TypeNode* type = nullptr;
    const ExprNode* expr;
  };
  Initializer init;
  const StatementNode* body;
};

struct ForLoopStatementNode : public StatementNode {
  ForLoopStatementNode() : StatementNode(StatementKind::ForLoopStatement) {}

  const StatementNode* init;
  const ExprNode* cond;
  const StatementNode* post;
  const StatementNode* body;
};

struct WhileLoopStatementNode : public StatementNode {
  WhileLoopStatementNode() : StatementNode(StatementKind::WhileLoopStatement) {}

  const ExprNode* cond;
  const StatementNode* body;
};

struct ReturnStatementNode : public StatementNode {
  ReturnStatementNode() : StatementNode(StatementKind::ReturnStatement) {}

  const ExprNode* expr = nullptr;
};

struct BreakStatementNode : public StatementNode {
  BreakStatementNode(std::string_view source_)
      : StatementNode(StatementKind::BreakStatement) { source = source_; }
};

struct ContinueStatementNode : public StatementNode {
  ContinueStatementNode(std::string_view source_)
      : StatementNode(StatementKind::ContinueStatement) { source = source_; }
};

struct TypeAliasStatementNode : public StatementNode {
  TypeAliasStatementNode() : StatementNode(StatementKind::TypeAliasStatement) {}

  const IdentifierNode* lhs;
  const TypeNode* rhs;
};

struct ProgramNode : public Node {
  const char* describe() const final { return "Program"; }

  Refs<StatementNode> statements;
};

} // namespace ast

namespace parser {

using namespace ast;

struct Diagnostic {
  size_t pos;
  std::string error;
};

Ptr<ProgramNode> parse(
    const std::string& input, std::vector<Diagnostic>* diagnostics);

std::string formatAST(const Node& node);

std::string formatDiagnostics(
    const std::string& input, std::vector<Diagnostic>* diagnostics);

} // namespace parser
