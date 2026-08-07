#ifndef UNIFY_H
#define UNIFY_H

#include <stdexcept>
#include <string>
#include <unordered_map>

#include "types.h"

class type_error : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

// Hindley-Milner unification over the type IR, with Remy-style row unification
// and levels for let-polymorphism.
class type_env {
public:
    // A type variable at this level is universally quantified.
    static constexpr int quantified_level = -1;

    // Returns a fresh type variable at the current level.
    type_ptr fresh_var();

    // Follows union-find bindings to the representative type.
    type_ptr find(type_ptr t) const;

    // Throws type_error when the two types cannot be made equal.
    void unify(type_ptr a, type_ptr b);

    // Quantifies the free variables of `t` above `level`, in place.
    type_ptr generalize(type_ptr t, int level);

    // Returns a copy of `scheme` with its quantified variables replaced by fresh
    // ones, so each use site gets independent variables.
    type_ptr instantiate(type_ptr scheme);

    void enter_level() { ++current_level_; }
    void leave_level() { --current_level_; }
    [[nodiscard]] int current_level() const noexcept { return current_level_; }

private:
    int next_id_ = 0;
    int current_level_ = 0;

    bool occurs_in(int var_id, const type_ptr& t) const;
    // Both arguments are resolved and share a kind, which is never type_var.
    void unify_same_kind(const type_ptr& a, const type_ptr& b);
    void unify_rows(const type_ptr& a, const type_ptr& b);
    // `mapping` keeps one fresh variable per quantified id across the recursion.
    type_ptr instantiate_with(type_ptr t, std::unordered_map<int, type_ptr>& mapping);
};

// Parses `s` into a type, or returns a fresh variable when `s` is empty.
type_ptr type_or_fresh(type_env& env, const std::string& s);

#endif  // UNIFY_H
