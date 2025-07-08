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
    auto data = std::vector<int>{std::unordered_map<int,int>{{a, 1}, {b, 2}}, std::unordered_map<int,int>{{a, 1}, {b, 1}}, std::unordered_map<int,int>{{a, 0}, {b, 5}}};
    auto sorted = ([&]() {
    std::vector<std::pair<decltype(std::unordered_map<int,int>{{a, x.a}, {b, x.b}}), decltype(x)>>> __items;
    for (auto x : data) {
        __items.push_back({std::unordered_map<int,int>{{a, x.a}, {b, x.b}}, x});
    }
    std::sort(__items.begin(), __items.end(), [](auto &a, auto &b){ return a.first < b.first; });
    std::vector<decltype(x)> __res;
    for (auto &p : __items) __res.push_back(p.second);
    return __res;
})();
    print(sorted);
    return 0;
}
