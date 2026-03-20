#include "types.h"

#include <format>
#include <stdexcept>

// ── Primitive singletons ─────────────────────────────────────────────────────

static type_ptr make_prim(prim_kind k) {
    static auto i = std::make_shared<prim_type_t>(prim_kind::int_t);
    static auto b = std::make_shared<prim_type_t>(prim_kind::bool_t);
    static auto by = std::make_shared<prim_type_t>(prim_kind::byte_t);
    static auto f = std::make_shared<prim_type_t>(prim_kind::float_t);
    static auto d = std::make_shared<prim_type_t>(prim_kind::double_t);
    static auto c = std::make_shared<prim_type_t>(prim_kind::char_t);
    static auto s = std::make_shared<prim_type_t>(prim_kind::string_t);
    switch (k) {
        case prim_kind::int_t:
            return i;
        case prim_kind::bool_t:
            return b;
        case prim_kind::byte_t:
            return by;
        case prim_kind::float_t:
            return f;
        case prim_kind::double_t:
            return d;
        case prim_kind::char_t:
            return c;
        case prim_kind::string_t:
            return s;
    }
    __builtin_unreachable();
}

type_ptr prim_int() { return make_prim(prim_kind::int_t); }
type_ptr prim_bool() { return make_prim(prim_kind::bool_t); }
type_ptr prim_byte() { return make_prim(prim_kind::byte_t); }
type_ptr prim_float() { return make_prim(prim_kind::float_t); }
type_ptr prim_double() { return make_prim(prim_kind::double_t); }
type_ptr prim_char() { return make_prim(prim_kind::char_t); }
type_ptr prim_string() { return make_prim(prim_kind::string_t); }

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
    std::string result = "(";
    for (bool first = true; const auto& p : params) {
        if (!first) result += ", ";
        result += p->to_string();
        first = false;
    }
    result += ") -> " + ret->to_string();
    return result;
}

std::string tuple_type_t::to_string() const {
    std::string result = "(";
    for (bool first = true; const auto& e : elements) {
        if (!first) result += ", ";
        result += e->to_string();
        first = false;
    }
    result += ")";
    return result;
}

std::string generic_type_t::to_string() const {
    std::string result = name + "<";
    for (bool first = true; const auto& a : args) {
        if (!first) result += ", ";
        result += a->to_string();
        first = false;
    }
    result += ">";
    return result;
}

std::string row_type_t::to_string() const {
    std::string result = "{";
    for (bool first = true; const auto& e : entries) {
        if (!first) result += ", ";
        result += e.label + ": " + e.type->to_string();
        first = false;
    }
    if (tail) {
        result += " | " + tail->to_string();
    }
    result += "}";
    return result;
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

// Parse the opaque type strings stored in AST nodes into the type IR.
// Grammar is restricted: base_type | IDENTIFIER | IDENTIFIER<inner> |
//                        integer<N> | integer<+N>
type_ptr parse_type_string(const std::string& s) {
    if (s.empty()) return nullptr;

    // Primitive types
    if (s == "int") return prim_int();
    if (s == "bool") return prim_bool();
    if (s == "byte") return prim_byte();
    if (s == "float") return prim_float();
    if (s == "double") return prim_double();
    if (s == "char") return prim_char();
    if (s == "string") return prim_string();

    // Check for generic/sized-int: look for '<'
    auto lt = s.find('<');
    if (lt != std::string::npos) {
        auto gt = s.rfind('>');
        if (gt == std::string::npos || gt <= lt) {
            throw std::runtime_error(std::format("malformed type: '{}'", s));
        }
        std::string outer = s.substr(0, lt);
        std::string inner = s.substr(lt + 1, gt - lt - 1);

        // Sized integer: integer<N> or integer<+N>
        if (outer == "integer") {
            bool is_unsigned = false;
            std::string num_str = inner;
            if (!inner.empty() && inner[0] == '+') {
                is_unsigned = true;
                num_str = inner.substr(1);
            }
            int bits = std::stoi(num_str);
            return std::make_shared<sized_int_type_t>(bits, is_unsigned);
        }

        // Generic type: name<inner_type>
        type_ptr inner_type = parse_type_string(inner);
        if (!inner_type) {
            throw std::runtime_error(std::format("malformed generic type arg: '{}'", inner));
        }
        std::vector<type_ptr> args;
        args.push_back(std::move(inner_type));
        return std::make_shared<generic_type_t>(std::move(outer), std::move(args));
    }

    // Named type (user-defined struct/interface)
    return std::make_shared<named_type_t>(s);
}
