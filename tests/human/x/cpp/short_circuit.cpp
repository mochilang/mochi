#include <iostream>
bool boom(int a,int b){std::cout<<"boom"<<std::endl;return true;}
int main(){
    std::cout<<(false && boom(1,2)?"true":"false")<<std::endl;
    std::cout<<(true || boom(1,2)?"true":"false")<<std::endl;
    return 0;
}
