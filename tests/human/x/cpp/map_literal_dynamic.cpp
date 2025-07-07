#include <iostream>
#include <map>
#include <string>
int main(){
    int x=3; int y=4;
    std::map<std::string,int> m{{"a",x},{"b",y}};
    std::cout<<m["a"]<<" "<<m["b"]<<std::endl;
    return 0;
}
