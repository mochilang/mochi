#include <iostream>
#include <vector>
#include <map>
#include <string>
struct Person{std::string name;std::string city;};
int main(){
    std::vector<Person> people={{"Alice","Paris"},{"Bob","Hanoi"},{"Charlie","Paris"},{"Diana","Hanoi"},{"Eve","Paris"},{"Frank","Hanoi"},{"George","Paris"}};
    std::map<std::string,int> counts;
    for(const auto&p:people) counts[p.city]++;
    for(const auto&kv:counts){
        if(kv.second>=4)
            std::cout<<"{city:"<<kv.first<<", num:"<<kv.second<<"}\n";
    }
    return 0;
}
