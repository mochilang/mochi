#include <iostream>
#include <functional>
int main(){
    std::function<int(int)> square=[](int x){return x*x;};
    std::cout<<square(6)<<std::endl;
    return 0;
}
