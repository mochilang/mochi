#include <iostream>
#include <functional>
std::function<int(int)> makeAdder(int n){
    return [n](int x){ return x + n; };
}
int main(){
    auto add10 = makeAdder(10);
    std::cout << add10(7);
    return 0;
}
