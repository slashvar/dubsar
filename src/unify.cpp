#include "unify.h"

#include <algorithm>
#include <format>
#include <unordered_map>

// ── Helpers ──────────────────────────────────────────────────────────────────

namespace {
std::string mismatch(const type_ptr& a, const type_ptr& b) {
    return std::format("cannot unify '{}' with '{}'", a->to_string(), b->to_string());
}

std::string labels_of(const std::vector<row_entry>& entries) {
    std::string result;
    for (const auto& e : entries) {
        if (!result.empty()) result += ", ";
        result += e.label;
    }
    return result;
}
}  // namespace

type_ptr type_env::fresh_var() { return std::make_shared<type_var_t>(next_id_++, current_level_); }

type_ptr type_or_fresh(type_env& env, const std::string& s) {
    auto t = parse_type_string(s);
    return t ? t : env.fresh_var();
}

type_ptr type_env::find(type_ptr t) const {
    // Path splitting keeps chains short so repeated lookups stay near O(1).
    while (t->kind() == type_kind::type_var) {
        auto* tv = static_cast<type_var_t*>(t.get());
        if (!tv->bound) {
            break;
        }
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
    auto contains = [&](const auto& types) {
        return std::ranges::any_of(types, [&](const type_ptr& x) { return occurs_in(var_id, x); });
    };
    switch (resolved->kind()) {
        case type_kind::type_var:
            return static_cast<type_var_t*>(resolved.get())->id == var_id;
        case type_kind::fun: {
            auto* ft = static_cast<fun_type_t*>(resolved.get());
            return contains(ft->params) || occurs_in(var_id, ft->ret);
        }
        case type_kind::tuple:
            return contains(static_cast<tuple_type_t*>(resolved.get())->elements);
        case type_kind::generic:
            return contains(static_cast<generic_type_t*>(resolved.get())->args);
        case type_kind::row: {
            auto* rt = static_cast<row_type_t*>(resolved.get());
            return std::ranges::any_of(
                       rt->entries,
                       [&](const row_entry& e) { return occurs_in(var_id, e.type); }) ||
                   (rt->tail && occurs_in(var_id, rt->tail));
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

    // Binding a variable is symmetric, so normalise it onto `a`.
    if (b->kind() == type_kind::type_var) {
        std::swap(a, b);
    }
    if (a->kind() == type_kind::type_var) {
        auto* tv = static_cast<type_var_t*>(a.get());
        if (occurs_in(tv->id, b)) {
            throw type_error(
                std::format("infinite type: '{}' occurs in '{}'", a->to_string(), b->to_string()));
        }
        tv->bound = b;
        return;
    }

    if (a->kind() != b->kind()) {
        throw type_error(mismatch(a, b));
    }
    unify_same_kind(a, b);
}

void type_env::unify_same_kind(const type_ptr& a, const type_ptr& b) {
    switch (a->kind()) {
        case type_kind::prim:
            if (static_cast<prim_type_t*>(a.get())->prim !=
                static_cast<prim_type_t*>(b.get())->prim) {
                throw type_error(mismatch(a, b));
            }
            return;

        case type_kind::sized_int: {
            auto* sa = static_cast<sized_int_type_t*>(a.get());
            auto* sb = static_cast<sized_int_type_t*>(b.get());
            if (sa->bits != sb->bits || sa->is_unsigned != sb->is_unsigned) {
                throw type_error(mismatch(a, b));
            }
            return;
        }

        case type_kind::named:
            if (static_cast<named_type_t*>(a.get())->name !=
                static_cast<named_type_t*>(b.get())->name) {
                throw type_error(mismatch(a, b));
            }
            return;

        case type_kind::fun: {
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

        case type_kind::tuple: {
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

        case type_kind::generic: {
            auto* ga = static_cast<generic_type_t*>(a.get());
            auto* gb = static_cast<generic_type_t*>(b.get());
            if (ga->name != gb->name || ga->args.size() != gb->args.size()) {
                throw type_error(mismatch(a, b));
            }
            for (size_t i = 0; i < ga->args.size(); ++i) {
                unify(ga->args[i], gb->args[i]);
            }
            return;
        }

        case type_kind::row:
            unify_rows(a, b);
            return;

        case type_kind::type_var:
            break;  // Normalised away above.
    }
    __builtin_unreachable();
}

// ── Rémy-style row unification ───────────────────────────────────────────────

void type_env::unify_rows(const type_ptr& a, const type_ptr& b) {
    auto* ra = static_cast<row_type_t*>(a.get());
    auto* rb = static_cast<row_type_t*>(b.get());

    // Unify the shared labels; collect the labels present on one side only.
    std::vector<row_entry> only_a;
    std::vector<row_entry> only_b;
    for (const auto& e : ra->entries) {
        if (auto shared = rb->find_entry(e.label)) {
            unify(e.type, shared);
        } else {
            only_a.push_back(e);
        }
    }
    for (const auto& e : rb->entries) {
        if (!ra->find_entry(e.label)) {
            only_b.push_back(e);
        }
    }

    auto tail_a = ra->tail ? find(ra->tail) : nullptr;
    auto tail_b = rb->tail ? find(rb->tail) : nullptr;
    const bool a_open = tail_a && tail_a->kind() == type_kind::type_var;
    const bool b_open = tail_b && tail_b->kind() == type_kind::type_var;

    // An open tail absorbs the labels the other row has in excess; a closed row
    // cannot, so any excess facing it is a mismatch.
    if (!b_open && !only_a.empty()) {
        throw type_error(std::format("row mismatch: extra labels {}", labels_of(only_a)));
    }
    if (!a_open && !only_b.empty()) {
        throw type_error(std::format("row mismatch: extra labels {}", labels_of(only_b)));
    }

    // Both open: the tails share a fresh ρ so later extensions stay consistent.
    auto shared_tail = a_open && b_open ? fresh_var() : nullptr;
    if (a_open) {
        unify(tail_a, std::make_shared<row_type_t>(std::move(only_b), shared_tail));
    }
    if (b_open) {
        unify(tail_b, std::make_shared<row_type_t>(std::move(only_a), shared_tail));
    }
}

// ── Generalize / Instantiate ─────────────────────────────────────────────────

type_ptr type_env::generalize(type_ptr t, int level) {
    t = find(t);
    switch (t->kind()) {
        case type_kind::type_var: {
            auto* tv = static_cast<type_var_t*>(t.get());
            if (tv->level > level) {
                tv->level = quantified_level;
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
        case type_kind::prim:
        case type_kind::sized_int:
        case type_kind::named:
            return t;
    }
    __builtin_unreachable();
}

type_ptr type_env::instantiate(type_ptr scheme) {
    std::unordered_map<int, type_ptr> mapping;
    return instantiate_with(std::move(scheme), mapping);
}

type_ptr type_env::instantiate_with(type_ptr t, std::unordered_map<int, type_ptr>& mapping) {
    t = find(t);
    auto map_all = [&](const std::vector<type_ptr>& types) {
        std::vector<type_ptr> result;
        result.reserve(types.size());
        for (const auto& x : types) {
            result.push_back(instantiate_with(x, mapping));
        }
        return result;
    };

    switch (t->kind()) {
        case type_kind::type_var: {
            auto* tv = static_cast<type_var_t*>(t.get());
            if (tv->level != quantified_level) {
                return t;
            }
            auto [it, inserted] = mapping.try_emplace(tv->id, nullptr);
            if (inserted) {
                it->second = fresh_var();
            }
            return it->second;
        }
        case type_kind::fun: {
            auto* ft = static_cast<fun_type_t*>(t.get());
            return std::make_shared<fun_type_t>(map_all(ft->params),
                                                instantiate_with(ft->ret, mapping));
        }
        case type_kind::tuple:
            return std::make_shared<tuple_type_t>(
                map_all(static_cast<tuple_type_t*>(t.get())->elements));
        case type_kind::generic: {
            auto* gt = static_cast<generic_type_t*>(t.get());
            return std::make_shared<generic_type_t>(gt->name, map_all(gt->args));
        }
        case type_kind::row: {
            auto* rt = static_cast<row_type_t*>(t.get());
            std::vector<row_entry> entries;
            entries.reserve(rt->entries.size());
            for (const auto& e : rt->entries) {
                entries.push_back({e.label, instantiate_with(e.type, mapping)});
            }
            return std::make_shared<row_type_t>(
                std::move(entries), rt->tail ? instantiate_with(rt->tail, mapping) : nullptr);
        }
        case type_kind::prim:
        case type_kind::sized_int:
        case type_kind::named:
            return t;
    }
    __builtin_unreachable();
}
