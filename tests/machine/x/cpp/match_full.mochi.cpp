#include <iostream>
#include <vector>
#include <unordered_map>
#include <map>
#include <algorithm>
#include <numeric>

template<typename T> void print(const T& v){ std::cout << v; }
void print(const std::vector<int>& v){ for(size_t i=0;i<v.size();++i){ if(i) std::cout<<' '; std::cout<<v[i]; }}
void print(bool b){ std::cout<<(b?"true":"false"); }

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
