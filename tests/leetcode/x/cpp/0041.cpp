#include <iostream>
#include <vector>
using namespace std;

int firstMissingPositive(vector<int>& nums) {
    int n = (int)nums.size();
    int i = 0;
    while (i < n) {
        int v = nums[i];
        if (v >= 1 && v <= n && nums[v - 1] != v) {
            swap(nums[i], nums[v - 1]);
        } else {
            i++;
        }
    }
    for (int i = 0; i < n; i++) if (nums[i] != i + 1) return i + 1;
    return n + 1;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n;
        cin >> n;
        vector<int> nums(n);
        for (int i = 0; i < n; i++) cin >> nums[i];
        if (tc) cout << '\n';
        cout << firstMissingPositive(nums);
    }
    return 0;
}
