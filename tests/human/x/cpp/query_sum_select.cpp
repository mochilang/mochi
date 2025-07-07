#include <iostream>
#include <vector>
int main(){
    std::vector<int> nums={1,2,3};
    int sum=0;for(int n:nums) if(n>1) sum+=n;
    std::cout<<sum<<std::endl;
    return 0;
}
