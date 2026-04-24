#include <iostream>
#include <vector>
using namespace std;

long long sortCount(vector<long long>& a, int l, int r, vector<long long>& tmp) {
    if (r - l <= 1) return 0;
    int m = (l + r) / 2;
    long long cnt = sortCount(a, l, m, tmp) + sortCount(a, m, r, tmp);
    int j = m;
    for (int i = l; i < m; ++i) {
        while (j < r && a[i] > 2LL * a[j]) ++j;
        cnt += j - m;
    }
    int i = l;
    j = m;
    int k = l;
    while (i < m && j < r) tmp[k++] = (a[i] <= a[j] ? a[i++] : a[j++]);
    while (i < m) tmp[k++] = a[i++];
    while (j < r) tmp[k++] = a[j++];
    for (int x = l; x < r; ++x) a[x] = tmp[x];
    return cnt;
}

long long solve(vector<long long> nums) {
    vector<long long> tmp(nums.size());
    return sortCount(nums, 0, nums.size(), tmp);
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
        if (tc) cout << "\n\n";
        cout << solve(nums);
    }
    return 0;
}
