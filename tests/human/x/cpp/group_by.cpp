#include <iostream>
#include <vector>
#include <map>
#include <string>
struct Person{std::string name;int age;std::string city;};
int main(){
    std::vector<Person> people={{"Alice",30,"Paris"},{"Bob",15,"Hanoi"},{"Charlie",65,"Paris"},{"Diana",45,"Hanoi"},{"Eve",70,"Paris"},{"Frank",22,"Hanoi"}};
    std::map<std::string,std::pair<int,int>> stats; // city -> (sumAge,count)
    for(const auto&p:people){
        auto &entry=stats[p.city];
        entry.first+=p.age;
        entry.second+=1;
    }
    std::cout<<"--- People grouped by city ---\n";
    for(const auto&kv:stats){
        double avg=(double)kv.second.first/kv.second.second;
        std::cout<<kv.first<<": count="<<kv.second.second<<", avg_age="<<avg<<"\n";
    }
    return 0;
}
