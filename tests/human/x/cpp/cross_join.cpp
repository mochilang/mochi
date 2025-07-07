#include <iostream>
#include <vector>
#include <string>
struct Customer {int id; std::string name;};
struct Order {int id; int customerId; int total;};
int main(){
    std::vector<Customer> customers={{1,"Alice"},{2,"Bob"},{3,"Charlie"}};
    std::vector<Order> orders={{100,1,250},{101,2,125},{102,1,300}};
    std::cout << "--- Cross Join: All order-customer pairs ---\n";
    for(const auto &o:orders){
        for(const auto &c:customers){
            std::cout << "Order "<<o.id
                      << "(customerId:"<<o.customerId
                      << ", total:$"<<o.total
                      << ") paired with "<<c.name<<"\n";
        }
    }
    return 0;
}
