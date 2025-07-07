#include <iostream>
#include <vector>
#include <algorithm>
int main(){
    std::vector<int> nums={1,2,3};
    auto has=[&](int v){return std::find(nums.begin(),nums.end(),v)!=nums.end();};
    std::cout<<(has(2)?"true":"false")<<std::endl;
    std::cout<<(has(4)?"true":"false")<<std::endl;
    return 0;
}
