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

int main() {
    struct Person {
        std::string name;
        int age;
    };
    struct Book {
        std::string title;
        Person author;
    };
    auto book = ([&]() { Book __v; __v.title = std::string("Go"); __v.author = ([&]() { Person __v; __v.name = std::string("Bob"); __v.age = 42; return __v; })(); return __v; })();
    print(book.author.name);
    return 0;
}
