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
    auto m = std::unordered_map<std::string,int>{{1, "a"}, {2, "b"}};
    print((std::find(1.begin(), 1.end(), m) != 1.end()));
    print((std::find(3.begin(), 3.end(), m) != 3.end()));
    return 0;
}
