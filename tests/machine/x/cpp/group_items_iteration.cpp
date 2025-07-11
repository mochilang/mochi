#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
struct Data{std::string tag;int val;};
int main(){
    std::vector<Data> data={{"a",1},{"a",2},{"b",3}};
    std::map<std::string,std::vector<Data>> groups;
    for(const auto&d:data) groups[d.tag].push_back(d);
    struct Res{std::string tag;int total;};
    std::vector<Res> tmp;
    for(const auto&g:groups){
        int total=0;for(const auto&x:g.second) total+=x.val;
        tmp.push_back({g.first,total});
    }
    std::sort(tmp.begin(),tmp.end(),[](auto&a,auto&b){return a.tag<b.tag;});
    for(const auto&r:tmp){std::cout<<"{tag:"<<r.tag<<", total:"<<r.total<<"}\n";}
    return 0;
}
