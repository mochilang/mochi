#include <iostream>
int k=2;
int inc(int x){return x+k;}
int main(){
    std::cout<<inc(3)<<std::endl;
    return 0;
}
