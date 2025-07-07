#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
struct Product{std::string name; int price;};
int main(){
    std::vector<Product> products={{"Laptop",1500},{"Smartphone",900},{"Tablet",600},{"Monitor",300},{"Keyboard",100},{"Mouse",50},{"Headphones",200}};
    std::sort(products.begin(),products.end(),[](const Product&a,const Product&b){return a.price>b.price;});
    std::cout<<"--- Top products (excluding most expensive) ---\n";
    for(size_t i=1;i<products.size() && i<4;i++){
        const auto&p=products[i];
        std::cout<<p.name<<" costs $"<<p.price<<"\n";
    }
    return 0;
}
