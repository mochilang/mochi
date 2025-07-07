#include <iostream>
#include <vector>
#include <algorithm>
int main(){
    std::vector<int> nums={3,1,4};
    std::cout<<*std::min_element(nums.begin(),nums.end())<<std::endl;
    std::cout<<*std::max_element(nums.begin(),nums.end())<<std::endl;
    return 0;
}
