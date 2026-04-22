#include <iostream>
#include <queue>
#include <vector>

using namespace std;

int shortestDistance(const vector<vector<int>>& grid) {
    int rows = grid.size(), cols = grid[0].size();
    vector<vector<int>> dist(rows, vector<int>(cols, 0));
    vector<vector<int>> reach(rows, vector<int>(cols, 0));
    int buildings = 0;
    for (int sr = 0; sr < rows; ++sr) {
        for (int sc = 0; sc < cols; ++sc) {
            if (grid[sr][sc] != 1) continue;
            buildings++;
            vector<vector<bool>> seen(rows, vector<bool>(cols, false));
            queue<tuple<int, int, int>> q;
            q.push({sr, sc, 0});
            seen[sr][sc] = true;
            while (!q.empty()) {
                auto [r, c, d] = q.front();
                q.pop();
                const int dr[4] = {1, -1, 0, 0};
                const int dc[4] = {0, 0, 1, -1};
                for (int k = 0; k < 4; ++k) {
                    int nr = r + dr[k], nc = c + dc[k];
                    if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && !seen[nr][nc]) {
                        seen[nr][nc] = true;
                        if (grid[nr][nc] == 0) {
                            dist[nr][nc] += d + 1;
                            reach[nr][nc] += 1;
                            q.push({nr, nc, d + 1});
                        }
                    }
                }
            }
        }
    }
    int ans = -1;
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            if (grid[r][c] == 0 && reach[r][c] == buildings) {
                if (ans == -1 || dist[r][c] < ans) ans = dist[r][c];
            }
        }
    }
    return ans;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int r, c;
        cin >> r >> c;
        vector<vector<int>> grid(r, vector<int>(c));
        for (int i = 0; i < r; ++i)
            for (int j = 0; j < c; ++j)
                cin >> grid[i][j];
        if (tc) cout << "\n\n";
        cout << shortestDistance(grid);
    }
    return 0;
}
