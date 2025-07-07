#include <iostream>
#include <map>
#include <string>
int main(){
    std::map<std::string,int> m={{"a",1},{"b",2}};
    std::cout<<m["b"]<<std::endl;
    return 0;
}
