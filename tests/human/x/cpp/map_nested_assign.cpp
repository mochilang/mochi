#include <iostream>
#include <map>
#include <string>
int main(){
    std::map<std::string,std::map<std::string,int>> data={{"outer",{{"inner",1}}}};
    data["outer"]["inner"]=2;
    std::cout<<data["outer"]["inner"]<<std::endl;
    return 0;
}
