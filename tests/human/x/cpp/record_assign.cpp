#include <iostream>
struct Counter{int n;};
void inc(Counter &c){c.n=c.n+1;}
int main(){
    Counter c{0};
    inc(c);
    std::cout<<c.n<<std::endl;
    return 0;
}
