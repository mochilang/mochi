#include <iostream>
#include <vector>

using namespace std;

int maxCoins(const vector<int>& nums) {
    vector<int> vals;
    vals.push_back(1);
    vals.insert(vals.end(), nums.begin(), nums.end());
    vals.push_back(1);
    int n = vals.size();
    vector<vector<int>> dp(n, vector<int>(n, 0));
    for (int length = 2; length < n; ++length) {
        for (int left = 0; left + length < n; ++left) {
            int right = left + length;
            for (int k = left + 1; k < right; ++k) {
                dp[left][right] = max(dp[left][right], dp[left][k] + dp[k][right] + vals[left] * vals[k] * vals[right]);
            }
        }
    }
    return dp[0][n - 1];
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n;
        cin >> n;
        vector<int> nums(n);
        for (int i = 0; i < n; ++i) cin >> nums[i];
        if (tc) cout << "\n\n";
        cout << maxCoins(nums);
    }
    return 0;
}
