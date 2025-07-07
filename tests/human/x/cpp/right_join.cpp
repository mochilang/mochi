#include <iostream>
#include <vector>
#include <string>
struct Customer{int id;std::string name;};
struct Order{int id;int customerId;int total;};
int main(){
    std::vector<Customer> customers={{1,"Alice"},{2,"Bob"},{3,"Charlie"},{4,"Diana"}};
    std::vector<Order> orders={{100,1,250},{101,2,125},{102,1,300}};
    std::cout<<"--- Right Join using syntax ---\n";
    for(const auto&c:customers){
        const Order* found=nullptr;
        for(const auto&o:orders) if(o.customerId==c.id){found=&o;break;}
        if(found) std::cout<<"Customer "<<c.name<<" has order "<<found->id<<" - $"<<found->total<<"\n";
        else std::cout<<"Customer "<<c.name<<" has no orders"<<"\n";
    }
    return 0;
}
