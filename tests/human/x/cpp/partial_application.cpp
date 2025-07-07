#include <iostream>
#include <functional>
int add(int a,int b){return a+b;}
int main(){
    auto add5=std::bind(add,5,std::placeholders::_1);
    std::cout<<add5(3)<<std::endl;
    return 0;
}
