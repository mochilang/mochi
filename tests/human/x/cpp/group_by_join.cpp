#include <iostream>
#include <vector>
#include <map>
#include <string>
struct Customer{int id;std::string name;};
struct Order{int id;int customerId;};
int main(){
    std::vector<Customer> customers={{1,"Alice"},{2,"Bob"}};
    std::vector<Order> orders={{100,1},{101,1},{102,2}};
    std::map<std::string,int> counts;
    for(const auto&o:orders){
        for(const auto&c:customers){
            if(o.customerId==c.id){counts[c.name]++;break;}
        }
    }
    std::cout<<"--- Orders per customer ---\n";
    for(const auto&kv:counts){
        std::cout<<kv.first<<" orders:"<<kv.second<<"\n";
    }
    return 0;
}
