#include <iostream>
#include <vector>
#include <unordered_map>
#include <map>
#include <algorithm>
#include <numeric>

template<typename T> void print(const T& v){ std::cout << v; }
void print(const std::vector<int>& v){ for(size_t i=0;i<v.size();++i){ if(i) std::cout<<' '; std::cout<<v[i]; }}
void print(bool b){ std::cout<<(b?"true":"false"); }

int main() {
    auto nums = std::vector<int>{3, 1, 4};
    print((*std::min_element(nums.begin(), nums.end())));
    print((*std::max_element(nums.begin(), nums.end())));
    return 0;
}
