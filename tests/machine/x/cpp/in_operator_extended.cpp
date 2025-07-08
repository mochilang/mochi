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
    auto xs = std::vector<int>{1, 2, 3};
    auto ys = ([&]() {
    std::vector<decltype(x)> __items;
    for (auto x : xs) {
        if (!(((x % 2) == 1))) continue;
        __items.push_back(x);
    }
    return __items;
})();
    print((std::find(ys.begin(), ys.end(), 1) != ys.end()));
    print((std::find(ys.begin(), ys.end(), 2) != ys.end()));
    auto m = std::unordered_map<int,int>{{a, 1}};
    print((m.count(std::string("a")) > 0));
    print((m.count(std::string("b")) > 0));
    auto s = std::string("hello");
    print((s.find(std::string("ell")) != std::string::npos));
    print((s.find(std::string("foo")) != std::string::npos));
    return 0;
}
