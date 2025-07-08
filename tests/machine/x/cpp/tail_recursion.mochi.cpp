#include <iostream>
#include <vector>
#include <unordered_map>
#include <map>
#include <algorithm>
#include <numeric>

template<typename T> void print(const T& v){ std::cout << v; }
void print(const std::vector<int>& v){ for(size_t i=0;i<v.size();++i){ if(i) std::cout<<' '; std::cout<<v[i]; }}
void print(bool b){ std::cout<<(b?"true":"false"); }

auto sum_rec(auto n, auto acc) {
    if ((n == 0)) {
        return acc;
    }
    return sum_rec((n - 1), (acc + n));
}

int main() {
    print(sum_rec(10, 0));
    return 0;
}
