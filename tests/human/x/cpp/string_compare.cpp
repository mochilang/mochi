#include <iostream>
#include <string>
int main(){
    std::cout<<(std::string("a")<"b"?"true":"false")<<std::endl;
    std::cout<<(std::string("a")<="a"?"true":"false")<<std::endl;
    std::cout<<(std::string("b")>"a"?"true":"false")<<std::endl;
    std::cout<<(std::string("b")>="b"?"true":"false")<<std::endl;
    return 0;
}
