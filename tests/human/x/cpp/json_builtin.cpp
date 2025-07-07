#include <iostream>
#include <map>
#include <string>
int main(){
    std::map<std::string,int> m{{"a",1},{"b",2}};
    std::cout<<"{";
    bool first=true;for(const auto&kv:m){if(!first)std::cout<<",";first=false;std::cout<<"\""<<kv.first<<"\":"<<kv.second;}
    std::cout<<"}"<<std::endl;
    return 0;
}
