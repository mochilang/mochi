#include <iostream>
#include <map>
int main(){
    std::map<int,std::string> m{{1,"a"},{2,"b"}};
    std::cout<<(m.count(1)?"true":"false")<<std::endl;
    std::cout<<(m.count(3)?"true":"false")<<std::endl;
    return 0;
}
