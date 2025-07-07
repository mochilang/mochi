#include <iostream>
#include <map>
#include <string>
int main(){
    std::map<std::string,int> m={{"a",1},{"b",2}};
    for(const auto &p:m){
        std::cout<<p.first<<std::endl;
    }
    return 0;
}
