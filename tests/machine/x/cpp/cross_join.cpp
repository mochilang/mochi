#include <iostream>
#include <vector>
#include <unordered_map>
#include <map>
#include <algorithm>
#include <numeric>

template<typename T> void print_val(const T& v){ std::cout << v; }
void print_val(const std::vector<int>& v){ for(size_t i=0;i<v.size();++i){ if(i) std::cout<<' '; std::cout<<v[i]; }}
void print_val(bool b){ std::cout<<(b?"true":"false"); }
void print(){ std::cout<<std::endl; }
template<typename First, typename... Rest> void print(const First& first, const Rest&... rest){ print_val(first); if constexpr(sizeof...(rest)>0){ std::cout<<' '; print(rest...); } else { std::cout<<std::endl; }}

int main() {
    auto customers = std::vector<int>{std::unordered_map<int,int>{{id, 1}, {name, std::string("Alice")}}, std::unordered_map<int,int>{{id, 2}, {name, std::string("Bob")}}, std::unordered_map<int,int>{{id, 3}, {name, std::string("Charlie")}}};
    auto orders = std::vector<int>{std::unordered_map<int,int>{{id, 100}, {customerId, 1}, {total, 250}}, std::unordered_map<int,int>{{id, 101}, {customerId, 2}, {total, 125}}, std::unordered_map<int,int>{{id, 102}, {customerId, 1}, {total, 300}}};
    auto result = ([&]() {
    std::vector<decltype(std::unordered_map<int,int>{{orderId, o.id}, {orderCustomerId, o.customerId}, {pairedCustomerName, c.name}, {orderTotal, o.total}})> __items;
    for (auto o : orders) {
        for (auto c : customers) {
            __items.push_back(std::unordered_map<int,int>{{orderId, o.id}, {orderCustomerId, o.customerId}, {pairedCustomerName, c.name}, {orderTotal, o.total}});
        }
    }
    return __items;
})();
    print(std::string("--- Cross Join: All order-customer pairs ---"));
    for (auto entry : result) {
        print(std::string("Order"), entry.orderId, std::string("(customerId:"), entry.orderCustomerId, std::string(", total: $"), entry.orderTotal, std::string(") paired with"), entry.pairedCustomerName);
    }
    return 0;
}
