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
    auto data = std::unordered_map<std::string,int>{{"outer", std::unordered_map<std::string,int>{{"inner", 1}}}};
    data["outer"]["inner"] = 2;
    print(data["outer"]["inner"]);
    return 0;
}
