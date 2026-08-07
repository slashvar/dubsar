#include "types.h"

#include <format>
#include <stdexcept>

// ── Primitive singletons ─────────────────────────────────────────────────────

namespace {
template <prim_kind K>
type_ptr prim() {
    static const type_ptr t = std::make_shared<prim_type_t>(K);
    return t;
}

// Join `items` with ", ", rendering each element through `to_str`.
template <typename Range, typename Fn>
std::string join(const Range& items, Fn to_str) {
    std::string result;
    for (bool first = true; const auto& item : items) {
        if (!first) result += ", ";
        result += to_str(item);
        first = false;
    }
    return result;
}

std::string join(const std::vector<type_ptr>& types) {
    return join(types, [](const type_ptr& t) { return t->to_string(); });
}
}  // namespace

type_ptr prim_int() { return prim<prim_kind::int_t>(); }
type_ptr prim_bool() { return prim<prim_kind::bool_t>(); }
type_ptr prim_byte() { return prim<prim_kind::byte_t>(); }
type_ptr prim_float() { return prim<prim_kind::float_t>(); }
type_ptr prim_double() { return prim<prim_kind::double_t>(); }
type_ptr prim_char() { return prim<prim_kind::char_t>(); }
type_ptr prim_string() { return prim<prim_kind::string_t>(); }

// ── to_string implementations ────────────────────────────────────────────────

std::string prim_type_t::to_string() const {
    switch (prim) {
        case prim_kind::int_t:
            return "int";
        case prim_kind::bool_t:
            return "bool";
        case prim_kind::byte_t:
            return "byte";
        case prim_kind::float_t:
            return "float";
        case prim_kind::double_t:
            return "double";
        case prim_kind::char_t:
            return "char";
        case prim_kind::string_t:
            return "string";
    }
    __builtin_unreachable();
}

std::string sized_int_type_t::to_string() const {
    return is_unsigned ? std::format("integer<+{}>", bits) : std::format("integer<{}>", bits);
}

std::string type_var_t::to_string() const {
    if (bound) return bound->to_string();
    return std::format("'{}", id);
}

std::string fun_type_t::to_string() const {
    return std::format("({}) -> {}", join(params), ret->to_string());
}

std::string tuple_type_t::to_string() const { return std::format("({})", join(elements)); }

std::string generic_type_t::to_string() const { return std::format("{}<{}>", name, join(args)); }

std::string row_type_t::to_string() const {
    auto body =
        join(entries, [](const row_entry& e) { return e.label + ": " + e.type->to_string(); });
    if (tail) {
        return std::format("{{{} | {}}}", body, tail->to_string());
    }
    return std::format("{{{}}}", body);
}

std::string named_type_t::to_string() const { return name; }

type_ptr row_type_t::find_entry(const std::string& label) const {
    for (const auto& e : entries) {
        if (e.label == label) {
            return e.type;
        }
    }
    return nullptr;
}

// ── parse_type_string ────────────────────────────────────────────────────────

type_ptr parse_type_string(const std::string& s) {
    if (s.empty()) return nullptr;

    if (s == "int") return prim_int();
    if (s == "bool") return prim_bool();
    if (s == "byte") return prim_byte();
    if (s == "float") return prim_float();
    if (s == "double") return prim_double();
    if (s == "char") return prim_char();
    if (s == "string") return prim_string();

    auto lt = s.find('<');
    if (lt == std::string::npos) {
        return std::make_shared<named_type_t>(s);
    }

    auto gt = s.rfind('>');
    if (gt <= lt || gt == std::string::npos) {
        throw std::runtime_error(std::format("malformed type: '{}'", s));
    }
    std::string outer = s.substr(0, lt);
    std::string inner = s.substr(lt + 1, gt - lt - 1);

    if (outer == "integer") {
        const bool is_unsigned = inner.starts_with('+');
        return std::make_shared<sized_int_type_t>(std::stoi(inner.substr(is_unsigned ? 1 : 0)),
                                                  is_unsigned);
    }

    type_ptr inner_type = parse_type_string(inner);
    if (!inner_type) {
        throw std::runtime_error(std::format("malformed generic type arg: '{}'", inner));
    }
    return std::make_shared<generic_type_t>(std::move(outer),
                                            std::vector<type_ptr>{std::move(inner_type)});
}
