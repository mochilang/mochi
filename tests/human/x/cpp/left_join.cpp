#include <iostream>
#include <vector>
#include <string>
struct Customer{int id;std::string name;};
struct Order{int id;int customerId;int total;};
int main(){
    std::vector<Customer> customers={{1,"Alice"},{2,"Bob"}};
    std::vector<Order> orders={{100,1,250},{101,3,80}};
    std::cout<<"--- Left Join ---\n";
    for(const auto&o:orders){
        const Customer* found=nullptr;
        for(const auto&c:customers){if(c.id==o.customerId){found=&c;break;}}
        std::cout<<"Order "<<o.id<<" customer ";
        if(found) std::cout<<found->name; else std::cout<<"null";
        std::cout<<" total "<<o.total<<"\n";
    }
    return 0;
}
