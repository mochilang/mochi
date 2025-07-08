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
    auto numbers = std::vector<int>{1, 2, 3, 4, 5, 6, 7, 8, 9};
    for (auto n : numbers) {
        if (((n % 2) == 0)) {
            continue;
        }
        if ((n > 7)) {
            break;
        }
        print("odd number:", n);
    }
    return 0;
}
