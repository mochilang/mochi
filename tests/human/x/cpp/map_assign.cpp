#include <iostream>
#include <map>
#include <string>
int main(){
    std::map<std::string,int> scores={{"alice",1}};
    scores["bob"]=2;
    std::cout<<scores["bob"]<<std::endl;
    return 0;
}
