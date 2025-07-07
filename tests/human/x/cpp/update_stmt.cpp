#include <iostream>
#include <vector>
#include <string>
struct Person{std::string name;int age;std::string status;};
int main(){
    std::vector<Person> people={{"Alice",17,"minor"},{"Bob",25,"unknown"},{"Charlie",18,"unknown"},{"Diana",16,"minor"}};
    for(auto &p:people){if(p.age>=18){p.status="adult";p.age+=1;}}
    std::vector<Person> expected={{"Alice",17,"minor"},{"Bob",26,"adult"},{"Charlie",19,"adult"},{"Diana",16,"minor"}};
    bool ok=people.size()==expected.size();
    for(size_t i=0;i<people.size() && ok;i++){ok &= (people[i].name==expected[i].name && people[i].age==expected[i].age && people[i].status==expected[i].status);} 
    if(!ok) std::cout<<"test failed"<<std::endl; else std::cout<<"ok"<<std::endl;
    return 0;
}
