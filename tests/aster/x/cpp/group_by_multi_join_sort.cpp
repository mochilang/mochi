// Generated by Mochi transpiler v0.10.34 on 2025-07-22 00:00:41 GMT+7
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <numeric>
#include <sstream>
#include <iomanip>
#include <optional>
template_declaration
template_declaration
struct NationItem {
    int n_nationkey;
    std::string n_name;
    auto function_declarator;
};
struct CustomerItem {
    int c_custkey;
    std::string c_name;
    double c_acctbal;
    int c_nationkey;
    std::string c_address;
    std::string c_phone;
    std::string c_comment;
    auto function_declarator;
};
struct OrdersItem {
    int o_orderkey;
    int o_custkey;
    std::string o_orderdate;
    auto function_declarator;
};
struct LineitemItem {
    int l_orderkey;
    std::string l_returnflag;
    double l_extendedprice;
    double l_discount;
    auto function_declarator;
};
struct GKey {
    int c_custkey;
    std::string c_name;
    double c_acctbal;
    std::string c_address;
    std::string c_phone;
    std::string c_comment;
    std::string n_name;
    auto function_declarator;
};
struct GRow {
    CustomerItem c;
    OrdersItem o;
    LineitemItem l;
    NationItem n;
    auto function_declarator;
};
struct GGroup {
    GKey key;
    std::vector<GRow> items;
    auto function_declarator;
    auto function_declarator;
    auto function_declarator;
    size_t function_declarator;
};
struct ResultItem {
    int c_custkey;
    std::string c_name;
    double revenue;
    double c_acctbal;
    std::string n_name;
    std::string c_address;
    std::string c_phone;
    std::string c_comment;
    auto function_declarator;
};
std::ostream function_declarator() {
    os << '{' << "'n_nationkey': " << v.n_nationkey << ", " << "'n_name': '" << v.n_name << "'" + '}';
    return os;
}
std::ostream function_declarator() {
    os << '{' << "'c_custkey': " << v.c_custkey << ", " << "'c_name': '" << v.c_name << "'" << ", " << "'c_acctbal': " << std::fixed << std::setprecision(1) << v.c_acctbal << ", " << "'c_nationkey': " << v.c_nationkey << ", " << "'c_address': '" << v.c_address << "'" << ", " << "'c_phone': '" << v.c_phone << "'" << ", " << "'c_comment': '" << v.c_comment << "'" + '}';
    return os;
}
std::ostream function_declarator() {
    os << '{' << "'o_orderkey': " << v.o_orderkey << ", " << "'o_custkey': " << v.o_custkey << ", " << "'o_orderdate': '" << v.o_orderdate << "'" + '}';
    return os;
}
std::ostream function_declarator() {
    os << '{' << "'l_orderkey': " << v.l_orderkey << ", " << "'l_returnflag': '" << v.l_returnflag << "'" << ", " << "'l_extendedprice': " << std::fixed << std::setprecision(1) << v.l_extendedprice << ", " << "'l_discount': " << std::fixed << std::setprecision(1) << v.l_discount + '}';
    return os;
}
std::ostream function_declarator() {
    os << '{' << "'c_custkey': " << v.c_custkey << ", " << "'c_name': '" << v.c_name << "'" << ", " << "'c_acctbal': " << std::fixed << std::setprecision(1) << v.c_acctbal << ", " << "'c_address': '" << v.c_address << "'" << ", " << "'c_phone': '" << v.c_phone << "'" << ", " << "'c_comment': '" << v.c_comment << "'" << ", " << "'n_name': '" << v.n_name << "'" + '}';
    return os;
}
std::ostream function_declarator() {
    os << '{' << "'c': " << v.c << ", " << "'o': " << v.o << ", " << "'l': " << v.l << ", " << "'n': " << v.n + '}';
    return os;
}
std::ostream function_declarator() {
    os << '{' << "'key': " << v.key << ", " << "'items': " << v.items + '}';
    return os;
}
std::ostream function_declarator() {
    os << '{' << "'c_custkey': " << v.c_custkey << ", " << "'c_name': '" << v.c_name << "'" << ", " << "'revenue': " << std::fixed << std::setprecision(1) << v.revenue << ", " << "'c_acctbal': " << std::fixed << std::setprecision(1) << v.c_acctbal << ", " << "'n_name': '" << v.n_name << "'" << ", " << "'c_address': '" << v.c_address << "'" << ", " << "'c_phone': '" << v.c_phone << "'" << ", " << "'c_comment': '" << v.c_comment << "'" + '}';
    return os;
}
std::vector<NationItem> nation = std::vector{NationItem{.n_nationkey = 1, .n_name = std::string("BRAZIL")}};
std::vector<CustomerItem> customer = std::vector{CustomerItem{.c_custkey = 1, .c_name = std::string("Alice"), .c_acctbal = 100.0, .c_nationkey = 1, .c_address = std::string("123 St"), .c_phone = std::string("123-456"), .c_comment = std::string("Loyal")}};
std::vector<OrdersItem> orders = std::vector{OrdersItem{.o_orderkey = 1000, .o_custkey = 1, .o_orderdate = std::string("1993-10-15")}, OrdersItem{.o_orderkey = 2000, .o_custkey = 1, .o_orderdate = std::string("1994-01-02")}};
std::vector<LineitemItem> lineitem = std::vector{LineitemItem{.l_orderkey = 1000, .l_returnflag = std::string("R"), .l_extendedprice = 1000.0, .l_discount = 0.1}, LineitemItem{.l_orderkey = 2000, .l_returnflag = std::string("N"), .l_extendedprice = 500.0, .l_discount = 0.0}};
auto start_date = std::string("1993-10-01");
auto end_date = std::string("1994-01-01");
std::vector<ResultItem> result = ([] {
    std::vector<ResultItem> __items;
    std::vector<GGroup> __groups;
    for (auto c : customer) {
        for (auto o : orders) {
            for (auto l : lineitem) {
                for (auto n : nation) {
                    if (((((((o.o_orderdate >= start_date) && (o.o_orderdate < end_date)) && (l.l_returnflag == std::string("R"))) && (o.o_custkey == c.c_custkey)) && (l.l_orderkey == o.o_orderkey)) && (n.n_nationkey == c.c_nationkey))) {
                        GRow __row = {c, o, l, n};
                        auto __key = GKey{.c_custkey = c.c_custkey, .c_name = c.c_name, .c_acctbal = c.c_acctbal, .c_address = c.c_address, .c_phone = c.c_phone, .c_comment = c.c_comment, .n_name = n.n_name};
                        bool __found = false;
                        for (auto &__g : __groups) {
                            if (__g.key == __key) {
                                __g.items.push_back(__row);
                                assignment_expression;
                                break;
                            }
                        }
                        if (!__found) {
                            GGroup __g = {__key};
                            __g.items.push_back(__row);
                            __groups.push_back(__g);
                        }
                    }
                }
            }
        }
    }
    for (auto &__g : __groups) {
        GGroup g = __g;
        __items.push_back(ResultItem{.c_custkey = g.key.c_custkey, .c_name = g.key.c_name, .revenue = (compound_statement()), .c_acctbal = g.key.c_acctbal, .n_name = g.key.n_name, .c_address = g.key.c_address, .c_phone = g.key.c_phone, .c_comment = g.key.c_comment});
    }
    return __items;
}());
int main() {
    std::cout << result << std::endl;
    return 0;
}
