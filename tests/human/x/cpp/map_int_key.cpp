#include <iostream>
#include <map>
int main(){
    std::map<int,std::string> m={{1,"a"},{2,"b"}};
    std::cout<<m[1]<<std::endl;
    return 0;
}
