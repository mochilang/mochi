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

auto sum_tree(auto t) {
    return ([&]() { auto __v = t; if (__v == Leaf) return 0; else if (__v == Node(left, value, right)) return ((sum_tree(left) + value) + sum_tree(right)); return decltype(0){}; })();
}

int main() {
    auto t = ([&]() { Node __v; __v.left = Leaf; __v.value = 1; __v.right = ([&]() { Node __v; __v.left = Leaf; __v.value = 2; __v.right = Leaf; return __v; })(); return __v; })();
    print(sum_tree(t));
    return 0;
}
