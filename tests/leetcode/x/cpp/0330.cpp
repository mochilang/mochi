#include <iostream>
#include <vector>
using namespace std;

int minPatches(vector<int>& nums, int n) {
    long long miss = 1;
    int i = 0, patches = 0;
    while (miss <= n) {
        if (i < (int)nums.size() && nums[i] <= miss) miss += nums[i++];
        else { miss += miss; ++patches; }
    }
    return patches;
}

int main() {
    ios::sync_with_stdio(false); cin.tie(nullptr);
    int t; if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int size; cin >> size; vector<int> nums(size); for (int &x : nums) cin >> x; int n; cin >> n;
        if (tc) cout << "\n\n";
        cout << minPatches(nums, n);
    }
}
