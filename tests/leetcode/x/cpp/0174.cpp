#include <iostream>
#include <vector>
using namespace std;

static int solve(const vector<vector<int>>& dungeon) {
    int cols = (int)dungeon[0].size();
    const long long inf = (long long)4e18;
    vector<long long> dp(cols + 1, inf);
    dp[cols - 1] = 1;
    for (int i = (int)dungeon.size() - 1; i >= 0; --i) {
        for (int j = cols - 1; j >= 0; --j) {
            long long need = min(dp[j], dp[j + 1]) - dungeon[i][j];
            dp[j] = need <= 1 ? 1 : need;
        }
    }
    return (int)dp[0];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int rows, cols;
        cin >> rows >> cols;
        vector<vector<int>> dungeon(rows, vector<int>(cols));
        for (int i = 0; i < rows; ++i)
            for (int j = 0; j < cols; ++j)
                cin >> dungeon[i][j];
        cout << solve(dungeon);
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
