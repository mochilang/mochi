#include <iostream>
#include <vector>
#include <string>
int main(){
    std::vector<int> v={1,2,3};
    std::cout<<v[1]<<" "<<v[2]<<std::endl;
    std::cout<<v[0]<<" "<<v[1]<<std::endl;
    std::string s="hello";
    std::cout<<s.substr(1,3)<<std::endl;
    return 0;
}
