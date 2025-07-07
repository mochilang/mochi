#include <iostream>
#include <vector>
#include <fstream>
#include <sstream>
#include <string>
struct Person{std::string name;int age;std::string email;};
int main(){
    std::ifstream f("../../interpreter/valid/people.yaml");
    std::string line;std::vector<Person> people;Person cur;
    while(std::getline(f,line)){
        if(line.find("- name:")!=std::string::npos){if(!cur.name.empty()) people.push_back(cur);cur=Person();cur.name=line.substr(line.find(':')+2);}
        else if(line.find("age:")!=std::string::npos){cur.age=std::stoi(line.substr(line.find(':')+2));}
        else if(line.find("email:")!=std::string::npos){cur.email=line.substr(line.find(':')+2);}
    }
    if(!cur.name.empty()) people.push_back(cur);
    for(const auto&p:people){ if(p.age>=18) std::cout<<p.name<<" "<<p.email<<"\n"; }
    return 0;
}
