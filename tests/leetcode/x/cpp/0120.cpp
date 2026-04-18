#include <iostream>
#include <vector>
using namespace std;

static int solve(const vector<vector<int>>& tri) {
    vector<int> dp = tri.back();
    for (int i = (int)tri.size() - 2; i >= 0; --i)
        for (int j = 0; j <= i; ++j)
            dp[j] = tri[i][j] + min(dp[j], dp[j + 1]);
    return dp[0];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int rows; cin >> rows;
        vector<vector<int>> tri(rows);
        for (int r = 1; r <= rows; ++r) {
            tri[r-1].resize(r);
            for (int j = 0; j < r; ++j) cin >> tri[r-1][j];
        }
        cout << solve(tri);
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
