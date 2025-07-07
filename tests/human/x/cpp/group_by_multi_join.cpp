#include <iostream>
#include <vector>
#include <map>
struct Nation{int id;std::string name;};
struct Supplier{int id;int nation;};
struct PartSupp{int part;int supplier;double cost;int qty;};
int main(){
    std::vector<Nation> nations={{1,"A"},{2,"B"}};
    std::vector<Supplier> suppliers={{1,1},{2,2}};
    std::vector<PartSupp> partsupp={{100,1,10.0,2},{100,2,20.0,1},{200,1,5.0,3}};
    std::map<int,double> totals;
    for(const auto&ps:partsupp){
        for(const auto&s:suppliers){
            if(ps.supplier==s.id){
                for(const auto&n:nations){
                    if(s.nation==n.id && n.name=="A"){totals[ps.part]+=ps.cost*ps.qty;}
                }
            }
        }
    }
    for(const auto&kv:totals){
        std::cout<<"{part:"<<kv.first<<", total:"<<kv.second<<"}\n";
    }
    return 0;
}
