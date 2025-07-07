#include <iostream>
#include <vector>
#include <set>
#include <string>
struct Customer{int id;std::string name;};
struct Order{int id;int customerId;int total;};
int main(){
    std::vector<Customer> customers={{1,"Alice"},{2,"Bob"},{3,"Charlie"},{4,"Diana"}};
    std::vector<Order> orders={{100,1,250},{101,2,125},{102,1,300},{103,5,80}};
    std::cout<<"--- Outer Join using syntax ---\n";
    std::set<int> orderMatched;
    for(const auto&o:orders){
        bool matched=false;
        for(const auto&c:customers){
            if(o.customerId==c.id){
                std::cout<<"Order "<<o.id<<" by "<<c.name<<" - $"<<o.total<<"\n";
                matched=true;orderMatched.insert(c.id);
            }
        }
        if(!matched){std::cout<<"Order "<<o.id<<" by Unknown - $"<<o.total<<"\n";}
    }
    for(const auto&c:customers){
        if(!orderMatched.count(c.id))
            std::cout<<"Customer "<<c.name<<" has no orders"<<"\n";
    }
    return 0;
}
