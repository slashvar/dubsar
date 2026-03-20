#ifndef UNIFY_H
#define UNIFY_H

#include <stdexcept>
#include <string>

#include "types.h"

// ── Type error ───────────────────────────────────────────────────────────────

class type_error : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

// ── Type environment (unification engine) ────────────────────────────────────

class type_env {
public:
    // Create a fresh type variable at the current level.
    type_ptr fresh_var();

    // Follow union-find chains to the representative type.
    type_ptr find(type_ptr t) const;

    // Unify two types.  Throws type_error on failure.
    void unify(type_ptr a, type_ptr b);

    // Generalize: replace free type vars at level > `level` with quantified
    // vars.  Returns a polymorphic type scheme.
    type_ptr generalize(type_ptr t, int level);

    // Instantiate: replace quantified vars with fresh type vars.
    type_ptr instantiate(type_ptr scheme);

    // Scope level management for let-polymorphism.
    void enter_level() { ++current_level_; }
    void leave_level() { --current_level_; }
    [[nodiscard]] int current_level() const noexcept { return current_level_; }

private:
    int next_id_ = 0;
    int current_level_ = 0;

    bool occurs_in(int var_id, const type_ptr& t) const;
    void unify_rows(const type_ptr& a, const type_ptr& b);
};

#endif  // UNIFY_H
