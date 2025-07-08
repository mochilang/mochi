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
    auto items = std::vector<int>{std::unordered_map<int,int>{{n, 1}, {v, std::string("a")}}, std::unordered_map<int,int>{{n, 1}, {v, std::string("b")}}, std::unordered_map<int,int>{{n, 2}, {v, std::string("c")}}};
    auto result = ([&]() {
    std::vector<std::pair<decltype(i.n), decltype(i.v)>>> __items;
    for (auto i : items) {
        __items.push_back({i.n, i.v});
    }
    std::sort(__items.begin(), __items.end(), [](auto &a, auto &b){ return a.first < b.first; });
    std::vector<decltype(i.v)> __res;
    for (auto &p : __items) __res.push_back(p.second);
    return __res;
})();
    print(result);
    return 0;
}
