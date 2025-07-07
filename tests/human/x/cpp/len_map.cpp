#include <iostream>
#include <map>
int main(){
    std::map<std::string,int> m{{"a",1},{"b",2}};
    std::cout << m.size();
    return 0;
}
