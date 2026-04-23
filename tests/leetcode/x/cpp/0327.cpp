#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>
using namespace std;

long long countRangeSum(vector<int>& nums, int lower, int upper) {
    vector<long long> pref(1, 0);
    for (int x : nums) pref.push_back(pref.back() + x);
    vector<long long> tmp(pref.size());
    function<long long(int,int)> solve = [&](int lo, int hi) -> long long {
        if (hi - lo <= 1) return 0;
        int mid = (lo + hi) / 2;
        long long ans = solve(lo, mid) + solve(mid, hi);
        int left = lo, right = lo;
        for (int r = mid; r < hi; ++r) {
            while (left < mid && pref[left] < pref[r] - upper) ++left;
            while (right < mid && pref[right] <= pref[r] - lower) ++right;
            ans += right - left;
        }
        merge(pref.begin() + lo, pref.begin() + mid, pref.begin() + mid, pref.begin() + hi, tmp.begin() + lo);
        copy(tmp.begin() + lo, tmp.begin() + hi, pref.begin() + lo);
        return ans;
    };
    return solve(0, pref.size());
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n; cin >> n;
        vector<int> nums(n);
        for (int &x : nums) cin >> x;
        int lower, upper; cin >> lower >> upper;
        if (tc) cout << "\n\n";
        cout << countRangeSum(nums, lower, upper);
    }
    return 0;
}
