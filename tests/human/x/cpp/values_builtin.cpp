#include <iostream>
#include <map>
#include <vector>
#include <string>
int main(){
    std::map<std::string,int> m={{"a",1},{"b",2},{"c",3}};
    std::vector<int> vals; for(auto&p:m) vals.push_back(p.second);
    for(size_t i=0;i<vals.size();++i){ if(i) std::cout<<" "; std::cout<<vals[i]; }
    return 0;
}
