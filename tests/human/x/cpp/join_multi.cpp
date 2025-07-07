#include <iostream>
#include <vector>
#include <string>
struct Customer{int id;std::string name;};
struct Order{int id;int customerId;};
struct Item{int orderId;std::string sku;};
int main(){
    std::vector<Customer> customers={{1,"Alice"},{2,"Bob"}};
    std::vector<Order> orders={{100,1},{101,2}};
    std::vector<Item> items={{100,"a"},{101,"b"}};
    std::cout<<"--- Multi Join ---\n";
    for(const auto&o:orders){
        for(const auto&c:customers){
            if(o.customerId==c.id){
                for(const auto&i:items){
                    if(i.orderId==o.id){
                        std::cout<<c.name<<" bought item "<<i.sku<<"\n";
                    }
                }
            }
        }
    }
    return 0;
}
