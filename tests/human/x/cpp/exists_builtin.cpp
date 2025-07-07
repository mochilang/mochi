#include <iostream>
#include <vector>
#include <algorithm>
int main(){
    std::vector<int> data={1,2};
    bool flag=std::any_of(data.begin(),data.end(),[](int x){return x==1;});
    std::cout<<(flag?"true":"false")<<std::endl;
    return 0;
}
