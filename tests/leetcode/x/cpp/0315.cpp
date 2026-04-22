#include <iostream>
#include <string>
#include <vector>

using namespace std;

static vector<int> countSmaller(const vector<int>& nums) {
    int n = nums.size();
    vector<int> counts(n, 0), idx(n), tmp(n);
    for (int i = 0; i < n; ++i) idx[i] = i;
    function<void(int, int)> sort = [&](int lo, int hi) {
        if (hi - lo <= 1) return;
        int mid = (lo + hi) / 2;
        sort(lo, mid);
        sort(mid, hi);
        int i = lo, j = mid, k = lo, moved = 0;
        while (i < mid && j < hi) {
            if (nums[idx[j]] < nums[idx[i]]) {
                tmp[k++] = idx[j++];
                moved++;
            } else {
                counts[idx[i]] += moved;
                tmp[k++] = idx[i++];
            }
        }
        while (i < mid) {
            counts[idx[i]] += moved;
            tmp[k++] = idx[i++];
        }
        while (j < hi) tmp[k++] = idx[j++];
        for (int p = lo; p < hi; ++p) idx[p] = tmp[p];
    };
    sort(0, n);
    return counts;
}

static string fmtList(const vector<int>& a) {
    string out = "[";
    for (int i = 0; i < (int)a.size(); ++i) {
        if (i) out += ",";
        out += to_string(a[i]);
    }
    out += "]";
    return out;
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
        cout << fmtList(countSmaller(nums));
    }
    return 0;
}
