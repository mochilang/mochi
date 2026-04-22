#include <cstdlib>
#include <iostream>
#include <unordered_map>
#include <vector>
using namespace std;

static long long bucket_id(long long x, long long size) {
    return x >= 0 ? x / size : -((-x - 1) / size) - 1;
}

static bool solve(const vector<long long>& nums, int indexDiff, long long valueDiff) {
    long long size = valueDiff + 1;
    unordered_map<long long, long long> buckets;
    for (int i = 0; i < (int)nums.size(); ++i) {
        long long x = nums[i];
        long long bid = bucket_id(x, size);
        if (buckets.count(bid)) return true;
        if (buckets.count(bid - 1) && llabs(x - buckets[bid - 1]) <= valueDiff) return true;
        if (buckets.count(bid + 1) && llabs(x - buckets[bid + 1]) <= valueDiff) return true;
        buckets[bid] = x;
        if (i >= indexDiff) buckets.erase(bucket_id(nums[i - indexDiff], size));
    }
    return false;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n;
        cin >> n;
        vector<long long> nums(n);
        for (int i = 0; i < n; ++i) cin >> nums[i];
        int indexDiff;
        long long valueDiff;
        cin >> indexDiff >> valueDiff;
        cout << (solve(nums, indexDiff, valueDiff) ? "true" : "false");
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
