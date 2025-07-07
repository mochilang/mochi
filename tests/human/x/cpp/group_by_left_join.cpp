#include <iostream>
#include <vector>
#include <map>
#include <string>
struct Customer{int id;std::string name;};
struct Order{int id;int customerId;};
int main(){
    std::vector<Customer> customers={{1,"Alice"},{2,"Bob"},{3,"Charlie"}};
    std::vector<Order> orders={{100,1},{101,1},{102,2}};
    std::map<std::string,int> counts;
    for(const auto&c:customers){counts[c.name]=0;}
    for(const auto&o:orders){
        for(const auto&c:customers){
            if(o.customerId==c.id){counts[c.name]++;break;}
        }
    }
    std::cout<<"--- Group Left Join ---\n";
    for(const auto&kv:counts){
        std::cout<<kv.first<<" orders:"<<kv.second<<"\n";
    }
    return 0;
}
