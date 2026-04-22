#include <iostream>
#include <vector>
#include <limits>

using namespace std;

static int solve(const vector<vector<int>>& costs) {
    if (costs.empty()) return 0;
    vector<int> prev = costs[0];
    for (size_t r = 1; r < costs.size(); ++r) {
        int min1 = numeric_limits<int>::max();
        int min2 = numeric_limits<int>::max();
        int idx1 = -1;
        for (int i = 0; i < (int)prev.size(); ++i) {
            if (prev[i] < min1) {
                min2 = min1;
                min1 = prev[i];
                idx1 = i;
            } else if (prev[i] < min2) {
                min2 = prev[i];
            }
        }
        vector<int> cur(prev.size());
        for (int i = 0; i < (int)prev.size(); ++i) {
            cur[i] = costs[r][i] + (i == idx1 ? min2 : min1);
        }
        prev.swap(cur);
    }
    int ans = prev[0];
    for (int v : prev) ans = min(ans, v);
    return ans;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n, k;
        cin >> n >> k;
        vector<vector<int>> costs(n, vector<int>(k));
        for (int i = 0; i < n; ++i) for (int j = 0; j < k; ++j) cin >> costs[i][j];
        if (tc) cout << '\n';
        cout << solve(costs);
    }
    return 0;
}
