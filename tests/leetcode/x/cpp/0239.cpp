#include <deque>
#include <iostream>
#include <vector>

using namespace std;

static vector<int> solve(const vector<int>& nums, int k) {
    deque<int> dq;
    vector<int> ans;
    for (int i = 0; i < (int)nums.size(); ++i) {
        while (!dq.empty() && dq.front() <= i - k) dq.pop_front();
        while (!dq.empty() && nums[dq.back()] <= nums[i]) dq.pop_back();
        dq.push_back(i);
        if (i >= k - 1) ans.push_back(nums[dq.front()]);
    }
    return ans;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n;
        cin >> n;
        vector<int> nums(n);
        for (int i = 0; i < n; ++i) cin >> nums[i];
        int k;
        cin >> k;
        vector<int> ans = solve(nums, k);
        if (tc) cout << "\n\n";
        cout << ans.size();
        for (int x : ans) cout << '\n' << x;
    }
    return 0;
}
