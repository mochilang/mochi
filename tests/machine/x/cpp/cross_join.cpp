#include <iostream>
#include <vector>
#include <unordered_map>
#include <map>
#include <algorithm>
#include <numeric>
#include <utility>

template<typename T> void print_val(const T& v){ std::cout << v; }
void print_val(const std::vector<int>& v){ for(size_t i=0;i<v.size();++i){ if(i) std::cout<<' '; std::cout<<v[i]; }}
void print_val(bool b){ std::cout<<(b?"true":"false"); }
void print(){ std::cout<<std::endl; }
template<typename First, typename... Rest> void print(const First& first, const Rest&... rest){ print_val(first); if constexpr(sizeof...(rest)>0){ std::cout<<' '; print(rest...); } else { std::cout<<std::endl; }}

struct __struct1 {decltype(1) id; decltype(std::string("Alice")) name; };
struct __struct2 {decltype(100) id; decltype(1) customerId; decltype(250) total; };
struct __struct3 {decltype(std::declval<__struct2>().id) orderId; decltype(std::declval<__struct2>().customerId) orderCustomerId; decltype(std::declval<__struct1>().name) pairedCustomerName; decltype(std::declval<__struct2>().total) orderTotal; };
int main() {
    auto customers = std::vector<decltype(__struct1{1, std::string("Alice")})>{__struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")}, __struct1{3, std::string("Charlie")}};
    auto orders = std::vector<decltype(__struct2{100, 1, 250})>{__struct2{100, 1, 250}, __struct2{101, 2, 125}, __struct2{102, 1, 300}};
    auto result = ([&]() {
    std::vector<__struct3> __items;
    for (auto o : orders) {
        for (auto c : customers) {
            __items.push_back(__struct3{o.id, o.customerId, c.name, o.total});
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
