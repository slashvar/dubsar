#include "unify.h"

#include <algorithm>
#include <format>
#include <functional>
#include <unordered_map>

// ── Helpers ──────────────────────────────────────────────────────────────────

type_ptr type_env::fresh_var() { return std::make_shared<type_var_t>(next_id_++, current_level_); }

type_ptr type_env::find(type_ptr t) const {
    // Path compression: flatten union-find chains so future lookups are O(1).
    while (t->kind() == type_kind::type_var) {
        auto* tv = static_cast<type_var_t*>(t.get());
        if (!tv->bound) {
            break;
        }
        // Skip one level (path splitting)
        if (tv->bound->kind() == type_kind::type_var) {
            auto* next = static_cast<type_var_t*>(tv->bound.get());
            if (next->bound) {
                tv->bound = next->bound;
            }
        }
        t = tv->bound;
    }
    return t;
}

bool type_env::occurs_in(int var_id, const type_ptr& t) const {
    auto resolved = find(t);
    switch (resolved->kind()) {
        case type_kind::type_var: {
            auto* tv = static_cast<type_var_t*>(resolved.get());
            return tv->id == var_id;
        }
        case type_kind::fun: {
            auto* ft = static_cast<fun_type_t*>(resolved.get());
            for (const auto& p : ft->params) {
                if (occurs_in(var_id, p)) {
                    return true;
                }
            }
            return occurs_in(var_id, ft->ret);
        }
        case type_kind::tuple: {
            auto* tt = static_cast<tuple_type_t*>(resolved.get());
            for (const auto& e : tt->elements) {
                if (occurs_in(var_id, e)) {
                    return true;
                }
            }
            return false;
        }
        case type_kind::generic: {
            auto* gt = static_cast<generic_type_t*>(resolved.get());
            for (const auto& a : gt->args) {
                if (occurs_in(var_id, a)) {
                    return true;
                }
            }
            return false;
        }
        case type_kind::row: {
            auto* rt = static_cast<row_type_t*>(resolved.get());
            for (const auto& e : rt->entries) {
                if (occurs_in(var_id, e.type)) {
                    return true;
                }
            }
            if (rt->tail) {
                return occurs_in(var_id, rt->tail);
            }
            return false;
        }
        case type_kind::prim:
        case type_kind::sized_int:
        case type_kind::named:
            return false;
    }
    __builtin_unreachable();
}

// ── Unification ──────────────────────────────────────────────────────────────

void type_env::unify(type_ptr a, type_ptr b) {
    a = find(a);
    b = find(b);

    if (a.get() == b.get()) {
        return;
    }

    // type_var binds
    if (a->kind() == type_kind::type_var) {
        auto* tv = static_cast<type_var_t*>(a.get());
        if (occurs_in(tv->id, b)) {
            throw type_error(
                std::format("infinite type: '{}' occurs in '{}'", a->to_string(), b->to_string()));
        }
        tv->bound = b;
        return;
    }
    if (b->kind() == type_kind::type_var) {
        auto* tv = static_cast<type_var_t*>(b.get());
        if (occurs_in(tv->id, a)) {
            throw type_error(
                std::format("infinite type: '{}' occurs in '{}'", b->to_string(), a->to_string()));
        }
        tv->bound = a;
        return;
    }

    // Primitives
    if (a->kind() == type_kind::prim && b->kind() == type_kind::prim) {
        auto* pa = static_cast<prim_type_t*>(a.get());
        auto* pb = static_cast<prim_type_t*>(b.get());
        if (pa->prim != pb->prim) {
            throw type_error(
                std::format("cannot unify '{}' with '{}'", a->to_string(), b->to_string()));
        }
        return;
    }

    // Sized integers
    if (a->kind() == type_kind::sized_int && b->kind() == type_kind::sized_int) {
        auto* sa = static_cast<sized_int_type_t*>(a.get());
        auto* sb = static_cast<sized_int_type_t*>(b.get());
        if (sa->bits != sb->bits || sa->is_unsigned != sb->is_unsigned) {
            throw type_error(
                std::format("cannot unify '{}' with '{}'", a->to_string(), b->to_string()));
        }
        return;
    }

    // Function types
    if (a->kind() == type_kind::fun && b->kind() == type_kind::fun) {
        auto* fa = static_cast<fun_type_t*>(a.get());
        auto* fb = static_cast<fun_type_t*>(b.get());
        if (fa->params.size() != fb->params.size()) {
            throw type_error(std::format("function arity mismatch: {} vs {} params",
                                         fa->params.size(), fb->params.size()));
        }
        for (size_t i = 0; i < fa->params.size(); ++i) {
            unify(fa->params[i], fb->params[i]);
        }
        unify(fa->ret, fb->ret);
        return;
    }

    // Tuple types
    if (a->kind() == type_kind::tuple && b->kind() == type_kind::tuple) {
        auto* ta = static_cast<tuple_type_t*>(a.get());
        auto* tb = static_cast<tuple_type_t*>(b.get());
        if (ta->elements.size() != tb->elements.size()) {
            throw type_error(std::format("tuple size mismatch: {} vs {} elements",
                                         ta->elements.size(), tb->elements.size()));
        }
        for (size_t i = 0; i < ta->elements.size(); ++i) {
            unify(ta->elements[i], tb->elements[i]);
        }
        return;
    }

    // Generic types
    if (a->kind() == type_kind::generic && b->kind() == type_kind::generic) {
        auto* ga = static_cast<generic_type_t*>(a.get());
        auto* gb = static_cast<generic_type_t*>(b.get());
        if (ga->name != gb->name || ga->args.size() != gb->args.size()) {
            throw type_error(
                std::format("cannot unify '{}' with '{}'", a->to_string(), b->to_string()));
        }
        for (size_t i = 0; i < ga->args.size(); ++i) {
            unify(ga->args[i], gb->args[i]);
        }
        return;
    }

    // Named types — same name means same type
    if (a->kind() == type_kind::named && b->kind() == type_kind::named) {
        auto* na = static_cast<named_type_t*>(a.get());
        auto* nb = static_cast<named_type_t*>(b.get());
        if (na->name == nb->name) {
            return;
        }
    }

    // Row types
    if (a->kind() == type_kind::row && b->kind() == type_kind::row) {
        unify_rows(a, b);
        return;
    }

    throw type_error(std::format("cannot unify '{}' with '{}'", a->to_string(), b->to_string()));
}

// ── Rémy-style row unification ───────────────────────────────────────────────

void type_env::unify_rows(const type_ptr& a, const type_ptr& b) {
    auto* ra = static_cast<row_type_t*>(a.get());
    auto* rb = static_cast<row_type_t*>(b.get());

    // Build label maps
    std::unordered_map<std::string, type_ptr> map_a;
    for (const auto& e : ra->entries) {
        map_a[e.label] = e.type;
    }
    std::unordered_map<std::string, type_ptr> map_b;
    for (const auto& e : rb->entries) {
        map_b[e.label] = e.type;
    }

    // Unify shared labels
    for (const auto& [label, type_a] : map_a) {
        if (auto it = map_b.find(label); it != map_b.end()) {
            unify(type_a, it->second);
        }
    }

    // Compute extra labels on each side
    std::vector<row_entry> only_a;
    for (const auto& e : ra->entries) {
        if (map_b.find(e.label) == map_b.end()) {
            only_a.push_back(e);
        }
    }
    std::vector<row_entry> only_b;
    for (const auto& e : rb->entries) {
        if (map_a.find(e.label) == map_a.end()) {
            only_b.push_back(e);
        }
    }

    auto tail_a = ra->tail ? find(ra->tail) : nullptr;
    auto tail_b = rb->tail ? find(rb->tail) : nullptr;
    bool a_open = tail_a && tail_a->kind() == type_kind::type_var;
    bool b_open = tail_b && tail_b->kind() == type_kind::type_var;

    if (!a_open && !b_open) {
        // Both closed: extra labels on either side is an error
        if (!only_a.empty() || !only_b.empty()) {
            std::string missing;
            for (const auto& e : only_a) {
                if (!missing.empty()) {
                    missing += ", ";
                }
                missing += e.label;
            }
            for (const auto& e : only_b) {
                if (!missing.empty()) {
                    missing += ", ";
                }
                missing += e.label;
            }
            throw type_error(std::format("row mismatch: extra labels {}", missing));
        }
    } else if (a_open && !b_open) {
        // a is open, b is closed: a's tail binds to b's extras
        if (!only_a.empty()) {
            throw type_error("closed row missing labels from open row");
        }
        auto new_row = std::make_shared<row_type_t>(std::move(only_b), nullptr);
        unify(tail_a, new_row);
    } else if (!a_open && b_open) {
        // b is open, a is closed: b's tail binds to a's extras
        if (!only_b.empty()) {
            throw type_error("closed row missing labels from open row");
        }
        auto new_row = std::make_shared<row_type_t>(std::move(only_a), nullptr);
        unify(tail_b, new_row);
    } else {
        // Both open: fresh ρ, bind both tails to extras + ρ
        auto fresh = fresh_var();
        auto row_for_a = std::make_shared<row_type_t>(std::move(only_b), fresh);
        auto row_for_b = std::make_shared<row_type_t>(std::move(only_a), fresh);
        unify(tail_a, row_for_a);
        unify(tail_b, row_for_b);
    }
}

// ── Generalize / Instantiate ─────────────────────────────────────────────────

// Generalize: mark free type vars at level > `level` as generic (level = -1).
type_ptr type_env::generalize(type_ptr t, int level) {
    t = find(t);
    switch (t->kind()) {
        case type_kind::type_var: {
            auto* tv = static_cast<type_var_t*>(t.get());
            if (tv->level > level) {
                tv->level = -1;  // Mark as generic/quantified
            }
            return t;
        }
        case type_kind::fun: {
            auto* ft = static_cast<fun_type_t*>(t.get());
            for (auto& p : ft->params) {
                p = generalize(p, level);
            }
            ft->ret = generalize(ft->ret, level);
            return t;
        }
        case type_kind::tuple: {
            auto* tt = static_cast<tuple_type_t*>(t.get());
            for (auto& e : tt->elements) {
                e = generalize(e, level);
            }
            return t;
        }
        case type_kind::generic: {
            auto* gt = static_cast<generic_type_t*>(t.get());
            for (auto& a : gt->args) {
                a = generalize(a, level);
            }
            return t;
        }
        case type_kind::row: {
            auto* rt = static_cast<row_type_t*>(t.get());
            for (auto& e : rt->entries) {
                e.type = generalize(e.type, level);
            }
            if (rt->tail) {
                rt->tail = generalize(rt->tail, level);
            }
            return t;
        }
        default:
            return t;
    }
}

// Instantiate: replace generic type vars (level == -1) with fresh vars.
type_ptr type_env::instantiate(type_ptr scheme) {
    std::unordered_map<int, type_ptr> mapping;

    std::function<type_ptr(type_ptr)> inst = [&](type_ptr t) -> type_ptr {
        t = find(t);
        switch (t->kind()) {
            case type_kind::type_var: {
                auto* tv = static_cast<type_var_t*>(t.get());
                if (tv->level == -1) {
                    auto it = mapping.find(tv->id);
                    if (it == mapping.end()) {
                        auto fv = fresh_var();
                        mapping[tv->id] = fv;
                        return fv;
                    }
                    return it->second;
                }
                return t;
            }
            case type_kind::fun: {
                auto* ft = static_cast<fun_type_t*>(t.get());
                std::vector<type_ptr> params;
                params.reserve(ft->params.size());
                for (const auto& p : ft->params) {
                    params.push_back(inst(p));
                }
                auto ret = inst(ft->ret);
                return std::make_shared<fun_type_t>(std::move(params), std::move(ret));
            }
            case type_kind::tuple: {
                auto* tt = static_cast<tuple_type_t*>(t.get());
                std::vector<type_ptr> elts;
                elts.reserve(tt->elements.size());
                for (const auto& e : tt->elements) {
                    elts.push_back(inst(e));
                }
                return std::make_shared<tuple_type_t>(std::move(elts));
            }
            case type_kind::generic: {
                auto* gt = static_cast<generic_type_t*>(t.get());
                std::vector<type_ptr> args;
                args.reserve(gt->args.size());
                for (const auto& a : gt->args) {
                    args.push_back(inst(a));
                }
                return std::make_shared<generic_type_t>(gt->name, std::move(args));
            }
            case type_kind::row: {
                auto* rt = static_cast<row_type_t*>(t.get());
                std::vector<row_entry> entries;
                entries.reserve(rt->entries.size());
                for (const auto& e : rt->entries) {
                    entries.push_back({e.label, inst(e.type)});
                }
                auto tail = rt->tail ? inst(rt->tail) : nullptr;
                return std::make_shared<row_type_t>(std::move(entries), std::move(tail));
            }
            default:
                return t;
        }
    };

    return inst(std::move(scheme));
}
