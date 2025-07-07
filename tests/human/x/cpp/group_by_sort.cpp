#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
struct Item{std::string cat;int val;};
int main(){
    std::vector<Item> items={{"a",3},{"a",1},{"b",5},{"b",2}};
    std::map<std::string,int> totals;
    for(const auto&i:items) totals[i.cat]+=i.val;
    std::vector<std::pair<std::string,int>> vec(totals.begin(),totals.end());
    std::sort(vec.begin(),vec.end(),[](auto&a,auto&b){return a.second>b.second;});
    for(const auto&kv:vec){
        std::cout<<"{cat:"<<kv.first<<", total:"<<kv.second<<"}\n";
    }
    return 0;
}
