#include <iostream>
#include <string>
#include <utility>
#include <vector>
using namespace std;

struct RangeModule {
    vector<pair<int, int>> intervals;

    void addRange(int left, int right) {
        vector<pair<int, int>> merged;
        int i = 0;
        while (i < (int)intervals.size() && intervals[i].second < left) {
            merged.push_back(intervals[i++]);
        }
        int newL = left, newR = right;
        while (i < (int)intervals.size() && intervals[i].first <= newR) {
            newL = min(newL, intervals[i].first);
            newR = max(newR, intervals[i].second);
            i++;
        }
        merged.push_back({newL, newR});
        while (i < (int)intervals.size()) merged.push_back(intervals[i++]);
        intervals = merged;
    }

    bool queryRange(int left, int right) {
        int lo = 0, hi = (int)intervals.size() - 1;
        while (lo <= hi) {
            int mid = (lo + hi) / 2;
            auto [start, end] = intervals[mid];
            if (start <= left) {
                if (right <= end) return true;
                lo = mid + 1;
            } else {
                hi = mid - 1;
            }
        }
        return false;
    }

    void removeRange(int left, int right) {
        vector<pair<int, int>> kept;
        for (auto [start, end] : intervals) {
            if (end <= left || right <= start) {
                kept.push_back({start, end});
            } else {
                if (start < left) kept.push_back({start, left});
                if (right < end) kept.push_back({right, end});
            }
        }
        intervals = kept;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int t;
    if (!(cin >> t)) return 0;
    vector<string> cases;
    while (t--) {
        int ops;
        cin >> ops;
        RangeModule rm;
        vector<string> out;
        for (int i = 0; i < ops; i++) {
            string op;
            cin >> op;
            if (op == "C") {
                rm = RangeModule();
                out.push_back("null");
            } else if (op == "A") {
                int l, r;
                cin >> l >> r;
                rm.addRange(l, r);
                out.push_back("null");
            } else if (op == "R") {
                int l, r;
                cin >> l >> r;
                rm.removeRange(l, r);
                out.push_back("null");
            } else {
                int l, r;
                cin >> l >> r;
                out.push_back(rm.queryRange(l, r) ? "true" : "false");
            }
        }
        string line = "[";
        for (int i = 0; i < (int)out.size(); i++) {
            if (i) line += ",";
            line += out[i];
        }
        line += "]";
        cases.push_back(line);
    }
    for (int i = 0; i < (int)cases.size(); i++) {
        if (i) cout << "\n\n";
        cout << cases[i];
    }
}
