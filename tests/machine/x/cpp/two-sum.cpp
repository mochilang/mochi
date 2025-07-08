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

auto twoSum(auto nums, auto target) {
    auto n = nums.size();
    for (int i = 0; i < n; ++i) {
        for (int j = (i + 1); j < n; ++j) {
            if (((nums[i] + nums[j]) == target)) {
                return std::vector<int>{i, j};
            }
        }
    }
    return std::vector<int>{(-1), (-1)};
}

int main() {
    auto result = twoSum(std::vector<int>{2, 7, 11, 15}, 9);
    print(result[0]);
    print(result[1]);
    return 0;
}
