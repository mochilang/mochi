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
    auto products = std::vector<int>{std::unordered_map<int,std::string>{{name, std::string("Laptop")}, {price, 1500}}, std::unordered_map<int,std::string>{{name, std::string("Smartphone")}, {price, 900}}, std::unordered_map<int,std::string>{{name, std::string("Tablet")}, {price, 600}}, std::unordered_map<int,std::string>{{name, std::string("Monitor")}, {price, 300}}, std::unordered_map<int,std::string>{{name, std::string("Keyboard")}, {price, 100}}, std::unordered_map<int,std::string>{{name, std::string("Mouse")}, {price, 50}}, std::unordered_map<int,std::string>{{name, std::string("Headphones")}, {price, 200}}};
    auto expensive = ([&]() {
    std::vector<std::pair<decltype((-p.price)), decltype(p)>>> __items;
    for (auto p : products) {
        __items.push_back({(-p.price), p});
    }
    std::sort(__items.begin(), __items.end(), [](auto &a, auto &b){ return a.first < b.first; });
    if ((size_t)1 < __items.size()) __items.erase(__items.begin(), __items.begin()+1);
    if ((size_t)3 < __items.size()) __items.resize(3);
    std::vector<decltype(p)> __res;
    for (auto &p : __items) __res.push_back(p.second);
    return __res;
})();
    print(std::string("--- Top products (excluding most expensive) ---"));
    for (auto item : expensive) {
        print(item.name, std::string("costs $"), item.price);
    }
    return 0;
}
