#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>
using namespace std;

int longestIncreasingPath(vector<vector<int>>& matrix) {
    int rows = matrix.size(), cols = matrix[0].size();
    vector<vector<int>> memo(rows, vector<int>(cols));
    int dirs[5] = {1, 0, -1, 0, 1};
    function<int(int,int)> dfs = [&](int r, int c) {
        if (memo[r][c]) return memo[r][c];
        int best = 1;
        for (int k = 0; k < 4; ++k) {
            int nr = r + dirs[k], nc = c + dirs[k + 1];
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && matrix[nr][nc] > matrix[r][c])
                best = max(best, 1 + dfs(nr, nc));
        }
        return memo[r][c] = best;
    };
    int ans = 0;
    for (int r = 0; r < rows; ++r) for (int c = 0; c < cols; ++c) ans = max(ans, dfs(r, c));
    return ans;
}

int main() {
    ios::sync_with_stdio(false); cin.tie(nullptr);
    int t; if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int rows, cols; cin >> rows >> cols;
        vector<vector<int>> m(rows, vector<int>(cols));
        for (auto &row : m) for (int &x : row) cin >> x;
        if (tc) cout << "\n\n";
        cout << longestIncreasingPath(m);
    }
}
