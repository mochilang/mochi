#include <iostream>
#include <vector>

using namespace std;

static int findMin(const vector<int>& nums) {
    int left = 0;
    int right = (int)nums.size() - 1;
    while (left < right) {
        int mid = (left + right) / 2;
        if (nums[mid] < nums[right]) right = mid;
        else if (nums[mid] > nums[right]) left = mid + 1;
        else right--;
    }
    return nums[left];
}

int main() {
    int tc;
    if (!(cin >> tc)) return 0;
    vector<int> out;
    for (int t = 0; t < tc; t++) {
        int n;
        cin >> n;
        vector<int> nums(n);
        for (int i = 0; i < n; i++) cin >> nums[i];
        out.push_back(findMin(nums));
    }
    for (int i = 0; i < (int)out.size(); i++) {
        if (i) cout << "\n\n";
        cout << out[i];
    }
    return 0;
}
