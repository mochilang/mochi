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
    auto nums = std::vector<int>{1, 2, 3};
    auto letters = std::vector<int>{std::string("A"), std::string("B")};
    auto pairs = ([&]() {
    std::vector<decltype(std::unordered_map<int,int>{{n, n}, {l, l}})> __items;
    for (auto n : nums) {
        for (auto l : letters) {
            if (!(((n % 2) == 0))) continue;
            __items.push_back(std::unordered_map<int,int>{{n, n}, {l, l}});
        }
    }
    return __items;
})();
    print(std::string("--- Even pairs ---"));
    for (auto p : pairs) {
        print(p.n, p.l);
    }
    return 0;
}
