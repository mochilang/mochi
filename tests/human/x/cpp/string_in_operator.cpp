#include <iostream>
#include <string>
int main(){
    std::string s="catch";
    std::cout<<(s.find("cat")!=std::string::npos?"true":"false")<<std::endl;
    std::cout<<(s.find("dog")!=std::string::npos?"true":"false")<<std::endl;
    return 0;
}
