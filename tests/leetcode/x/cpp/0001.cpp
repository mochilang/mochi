#include <iostream>
#include <vector>

using namespace std;

static pair<int, int> twoSum(const vector<int>& nums, int target) {
    for (int i = 0; i < (int)nums.size(); i++) {
        for (int j = i + 1; j < (int)nums.size(); j++) {
            if (nums[i] + nums[j] == target) {
                return {i, j};
            }
        }
    }
    return {0, 0};
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int t;
    if (!(cin >> t)) {
        return 0;
    }
    while (t--) {
        int n, target;
        cin >> n >> target;
        vector<int> nums(n);
        for (int i = 0; i < n; i++) {
            cin >> nums[i];
        }
        auto ans = twoSum(nums, target);
        cout << ans.first << ' ' << ans.second << "\n";
    }
    return 0;
}
