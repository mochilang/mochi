#include <algorithm>
#include <functional>
#include <iostream>
#include <queue>
#include <string>
#include <unordered_map>
#include <vector>
using namespace std;

struct DualHeap {
    priority_queue<int> small;
    priority_queue<int, vector<int>, greater<int>> large;
    unordered_map<int, int> delayed;
    int k;
    int smallSize = 0, largeSize = 0;

    explicit DualHeap(int k) : k(k) {}

    void pruneSmall() {
        while (!small.empty()) {
            int x = small.top();
            auto it = delayed.find(x);
            if (it == delayed.end() || it->second == 0) break;
            if (--it->second == 0) delayed.erase(it);
            small.pop();
        }
    }

    void pruneLarge() {
        while (!large.empty()) {
            int x = large.top();
            auto it = delayed.find(x);
            if (it == delayed.end() || it->second == 0) break;
            if (--it->second == 0) delayed.erase(it);
            large.pop();
        }
    }

    void rebalance() {
        if (smallSize > largeSize + 1) {
            large.push(small.top());
            small.pop();
            --smallSize;
            ++largeSize;
            pruneSmall();
        } else if (smallSize < largeSize) {
            small.push(large.top());
            large.pop();
            ++smallSize;
            --largeSize;
            pruneLarge();
        }
    }

    void add(int x) {
        if (small.empty() || x <= small.top()) {
            small.push(x);
            ++smallSize;
        } else {
            large.push(x);
            ++largeSize;
        }
        rebalance();
    }

    void remove(int x) {
        ++delayed[x];
        if (x <= small.top()) {
            --smallSize;
            if (x == small.top()) pruneSmall();
        } else {
            --largeSize;
            if (!large.empty() && x == large.top()) pruneLarge();
        }
        rebalance();
    }

    long long median2() {
        pruneSmall();
        pruneLarge();
        if (k % 2) return (long long)small.top() * 2;
        return (long long)small.top() + large.top();
    }
};

string fmtVal(long long v2) {
    if (v2 % 2 == 0) return to_string(v2 / 2);
    string sign = v2 < 0 ? "-" : "";
    long long a = llabs(v2);
    return sign + to_string(a / 2) + ".5";
}

string solve(const vector<int>& nums, int k) {
    DualHeap dh(k);
    for (int i = 0; i < k; ++i) dh.add(nums[i]);
    vector<string> out{fmtVal(dh.median2())};
    for (int i = k; i < (int)nums.size(); ++i) {
        dh.add(nums[i]);
        dh.remove(nums[i - k]);
        out.push_back(fmtVal(dh.median2()));
    }
    string s = "[";
    for (int i = 0; i < (int)out.size(); ++i) {
        if (i) s += ",";
        s += out[i];
    }
    s += "]";
    return s;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n, k;
        cin >> n >> k;
        vector<int> nums(n);
        for (int i = 0; i < n; ++i) cin >> nums[i];
        if (tc) cout << "\n\n";
        cout << solve(nums, k);
    }
    return 0;
}
