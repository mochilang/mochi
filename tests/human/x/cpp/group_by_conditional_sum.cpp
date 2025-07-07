#include <iostream>
#include <vector>
#include <map>
#include <string>
struct Item{std::string cat;int val;bool flag;};
int main(){
    std::vector<Item> items={{"a",10,true},{"a",5,false},{"b",20,true}};
    std::map<std::string,std::pair<int,int>> stats; // cat -> (sumTotal,sumFlag)
    for(const auto&i:items){
        auto &s=stats[i.cat];
        s.first+=i.val;
        if(i.flag) s.second+=i.val;
    }
    for(const auto&kv:stats){
        double share=kv.second.first? (double)kv.second.second/kv.second.first : 0.0;
        std::cout<<kv.first<<" share="<<share<<"\n";
    }
    return 0;
}
