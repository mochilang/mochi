#include <iostream>
#include <vector>
int main(){
    std::vector<int> nums{1,2};
    nums[1] = 3;
    std::cout << nums[1];
    return 0;
}
