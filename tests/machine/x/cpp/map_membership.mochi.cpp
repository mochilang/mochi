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
    auto m = std::unordered_map<std::string,int>{{"a", 1}, {"b", 2}};
    print((std::find("a".begin(), "a".end(), m) != "a".end()));
    print((std::find("c".begin(), "c".end(), m) != "c".end()));
    return 0;
}
