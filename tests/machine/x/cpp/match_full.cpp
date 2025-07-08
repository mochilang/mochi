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

auto classify(auto n) {
    return ([&]() { auto __v = n; if (__v == 0) return "zero"; else if (__v == 1) return "one"; return "many"; })();
}

int main() {
    auto x = 2;
    auto label = ([&]() { auto __v = x; if (__v == 1) return "one"; else if (__v == 2) return "two"; else if (__v == 3) return "three"; return "unknown"; })();
    print(label);
    auto day = "sun";
    auto mood = ([&]() { auto __v = day; if (__v == "mon") return "tired"; else if (__v == "fri") return "excited"; else if (__v == "sun") return "relaxed"; return "normal"; })();
    print(mood);
    auto ok = true;
    auto status = ([&]() { auto __v = ok; if (__v == true) return "confirmed"; else if (__v == false) return "denied"; return decltype("confirmed"){}; })();
    print(status);
    print(classify(0));
    print(classify(5));
    return 0;
}
