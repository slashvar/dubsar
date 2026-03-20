#ifndef TYPES_H
#define TYPES_H

#include <memory>
#include <string>
#include <vector>

// Forward declaration
class type_t;
using type_ptr = std::shared_ptr<type_t>;

// ── Type IR hierarchy ────────────────────────────────────────────────────────

enum class type_kind {
    prim,
    sized_int,
    type_var,
    fun,
    tuple,
    generic,
    row,
    named,
};

class type_t {
public:
    virtual ~type_t() = default;
    [[nodiscard]] virtual type_kind kind() const noexcept = 0;
    [[nodiscard]] virtual std::string to_string() const = 0;
};

// ── Primitive types (singletons) ─────────────────────────────────────────────

enum class prim_kind { int_t, bool_t, byte_t, float_t, double_t, char_t, string_t };

class prim_type_t : public type_t {
public:
    prim_kind prim;
    explicit prim_type_t(prim_kind p) noexcept : prim(p) {}
    [[nodiscard]] type_kind kind() const noexcept override { return type_kind::prim; }
    [[nodiscard]] std::string to_string() const override;
};

// Singleton accessors for primitive types.
type_ptr prim_int();
type_ptr prim_bool();
type_ptr prim_byte();
type_ptr prim_float();
type_ptr prim_double();
type_ptr prim_char();
type_ptr prim_string();

// ── Sized integer: integer<N> or integer<+N> ────────────────────────────────

class sized_int_type_t : public type_t {
public:
    int bits;
    bool is_unsigned;
    sized_int_type_t(int b, bool u) noexcept : bits(b), is_unsigned(u) {}
    [[nodiscard]] type_kind kind() const noexcept override { return type_kind::sized_int; }
    [[nodiscard]] std::string to_string() const override;
};

// ── Type variable (unification) ─────────────────────────────────────────────

class type_var_t : public type_t {
public:
    int id;
    int level;
    type_ptr bound;  // nullptr = free

    type_var_t(int id, int lvl) noexcept : id(id), level(lvl) {}
    [[nodiscard]] type_kind kind() const noexcept override { return type_kind::type_var; }
    [[nodiscard]] std::string to_string() const override;
};

// ── Function type: (T1, T2, ...) -> T_ret ───────────────────────────────────

class fun_type_t : public type_t {
public:
    std::vector<type_ptr> params;
    type_ptr ret;

    fun_type_t(std::vector<type_ptr> p, type_ptr r) : params(std::move(p)), ret(std::move(r)) {}
    [[nodiscard]] type_kind kind() const noexcept override { return type_kind::fun; }
    [[nodiscard]] std::string to_string() const override;
};

// ── Tuple type: (T1, T2, ...) ───────────────────────────────────────────────

class tuple_type_t : public type_t {
public:
    std::vector<type_ptr> elements;

    explicit tuple_type_t(std::vector<type_ptr> elts) : elements(std::move(elts)) {}
    [[nodiscard]] type_kind kind() const noexcept override { return type_kind::tuple; }
    [[nodiscard]] std::string to_string() const override;
};

// ── Generic type: name<T1, T2, ...> (e.g. vector<int>) ─────────────────────

class generic_type_t : public type_t {
public:
    std::string name;
    std::vector<type_ptr> args;

    generic_type_t(std::string n, std::vector<type_ptr> a)
        : name(std::move(n)), args(std::move(a)) {}
    [[nodiscard]] type_kind kind() const noexcept override { return type_kind::generic; }
    [[nodiscard]] std::string to_string() const override;
};

// ── Row type: { label1: T1, ... | tail } ────────────────────────────────────

struct row_entry {
    std::string label;
    type_ptr type;
};

class row_type_t : public type_t {
public:
    std::vector<row_entry> entries;
    type_ptr tail;  // nullptr = closed, type_var = open (ρ)

    row_type_t(std::vector<row_entry> e, type_ptr t) : entries(std::move(e)), tail(std::move(t)) {}
    [[nodiscard]] type_kind kind() const noexcept override { return type_kind::row; }
    [[nodiscard]] std::string to_string() const override;

    // Find an entry by label.  Returns nullptr if not found.
    [[nodiscard]] type_ptr find_entry(const std::string& label) const;
};

// ── Named type: reference to a declared struct/interface ────────────────────

class named_type_t : public type_t {
public:
    std::string name;

    explicit named_type_t(std::string n) : name(std::move(n)) {}
    [[nodiscard]] type_kind kind() const noexcept override { return type_kind::named; }
    [[nodiscard]] std::string to_string() const override;
};

// ── Parse helper ─────────────────────────────────────────────────────────────

// Convert opaque type strings (as stored in AST nodes) to type_ptr.
// Returns nullptr for empty strings (un-annotated / inferred).
type_ptr parse_type_string(const std::string& s);

#endif  // TYPES_H
