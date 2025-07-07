#include <iostream>
#include <map>
#include <string>
int main(){
    std::map<std::string,int> m={{"a",1},{"b",2}};
    std::cout<<(m.count("a")?"true":"false")<<std::endl;
    std::cout<<(m.count("c")?"true":"false")<<std::endl;
    return 0;
}
