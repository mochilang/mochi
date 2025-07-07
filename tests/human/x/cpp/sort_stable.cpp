#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
struct Item{int n;std::string v;};
int main(){
    std::vector<Item> items={{1,"a"},{1,"b"},{2,"c"}};
    std::stable_sort(items.begin(),items.end(),[](const Item&a,const Item&b){return a.n<b.n;});
    for(const auto&i:items) std::cout<<i.v<<" ";
    std::cout<<std::endl;return 0;
}
