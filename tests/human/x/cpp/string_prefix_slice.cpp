#include <iostream>
#include <string>
int main(){
    std::string prefix="fore";std::string s1="forest";std::cout<<(s1.substr(0,prefix.size())==prefix?"true":"false")<<std::endl;
    std::string s2="desert";std::cout<<(s2.substr(0,prefix.size())==prefix?"true":"false")<<std::endl;
    return 0;
}
