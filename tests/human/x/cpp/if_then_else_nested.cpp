#include <iostream>
int main(){
    int x=8;
    std::string msg=x>10?"big":(x>5?"medium":"small");
    std::cout<<msg<<std::endl;
    return 0;
}
