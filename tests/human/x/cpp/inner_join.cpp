#include <iostream>
#include <vector>
#include <string>
struct Customer{int id;std::string name;};
struct Order{int id;int customerId;int total;};
int main(){
    std::vector<Customer> customers={{1,"Alice"},{2,"Bob"},{3,"Charlie"}};
    std::vector<Order> orders={{100,1,250},{101,2,125},{102,1,300},{103,4,80}};
    std::cout<<"--- Orders with customer info ---\n";
    for(const auto&o:orders){
        for(const auto&c:customers){
            if(o.customerId==c.id){
                std::cout<<"Order "<<o.id<<" by "<<c.name<<" - $"<<o.total<<"\n";
            }
        }
    }
    return 0;
}
