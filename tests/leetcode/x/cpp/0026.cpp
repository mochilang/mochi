#include <iostream>
#include <vector>
#include <string>

class Solution {
public:
    int removeDuplicates(std::vector<int>& nums) {
        if (nums.empty()) return 0;
        int k = 1;
        for (int i = 1; i < nums.size(); ++i) {
            if (nums[i] != nums[k - 1]) {
                nums[k] = nums[i];
                k++;
            }
        }
        return k;
    }
};

int main() {
    int t;
    if (!(std::cin >> t)) return 0;
    Solution sol;
    while (t--) {
        int n;
        std::cin >> n;
        std::vector<int> nums(n);
        for (int i = 0; i < n; ++i) {
            std::cin >> nums[i];
        }
        int k = sol.removeDuplicates(nums);
        for (int i = 0; i < k; ++i) {
            std::cout << nums[i] << (i == k - 1 ? "" : " ");
        }
        std::cout << std::endl;
    }
    return 0;
}
