#include <iostream>
#include <vector>
#include <string>
struct Person{std::string name;int age;};
int main(){
    std::vector<Person> people={{"Alice",30},{"Bob",25}};
    for(const auto&p:people){
        std::cout<<"{\"name\":\""<<p.name<<"\",\"age\":"<<p.age<<"}"<<"\n";
    }
    return 0;
}
