#include <iostream>
#include <vector>
std::vector<int> twoSum(const std::vector<int>& nums, int target){
    for(size_t i=0;i<nums.size();++i){
        for(size_t j=i+1;j<nums.size();++j){
            if(nums[i]+nums[j]==target){
                return {static_cast<int>(i), static_cast<int>(j)};
            }
        }
    }
    return {-1,-1};
}
int main(){
    auto result = twoSum({2,7,11,15},9);
    std::cout << result[0] << std::endl;
    std::cout << result[1];
    return 0;
}
