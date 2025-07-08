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
    auto s = "catch";
    print((std::find("cat".begin(), "cat".end(), s) != "cat".end()));
    print((std::find("dog".begin(), "dog".end(), s) != "dog".end()));
    return 0;
}
