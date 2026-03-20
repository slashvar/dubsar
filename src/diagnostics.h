#ifndef DIAGNOSTICS_H
#define DIAGNOSTICS_H

#include <ostream>
#include <string>
#include <vector>

class ast_node;

enum class severity { error, warning };

class diagnostics {
public:
    void error(const ast_node& node, std::string message);
    void error(int line, std::string message);
    void warning(const ast_node& node, std::string message);
    void warning(int line, std::string message);
    [[nodiscard]] bool has_errors() const noexcept;
    [[nodiscard]] bool has_diagnostics() const noexcept;
    void emit(std::ostream& err) const;

private:
    struct diag_entry {
        severity sev;
        int line;
        std::string message;
    };
    std::vector<diag_entry> entries_;
};

#endif  // DIAGNOSTICS_H
