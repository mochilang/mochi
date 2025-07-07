#include <iostream>
#include <vector>
#include <string>
struct Person{std::string name; int age;};
int main(){
    std::vector<Person> people={{"Alice",30},{"Bob",15},{"Charlie",65},{"Diana",45}};
    std::cout<<"--- Adults ---\n";
    for(const auto&p:people){
        if(p.age>=18){
            bool is_senior=p.age>=60;
            std::cout<<p.name<<" is "<<p.age<<(is_senior?" (senior)":"")<<"\n";
        }
    }
    return 0;
}
