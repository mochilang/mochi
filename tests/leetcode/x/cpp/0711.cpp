#include <algorithm>
#include <iostream>
#include <set>
#include <string>
#include <utility>
#include <vector>
using namespace std;

string canonical(const vector<pair<int, int>>& cells) {
    vector<vector<pair<int, int>>> transforms(8);
    for (auto [x, y] : cells) {
        vector<pair<int, int>> variants = {
            {x, y}, {x, -y}, {-x, y}, {-x, -y},
            {y, x}, {y, -x}, {-y, x}, {-y, -x},
        };
        for (int i = 0; i < 8; i++) transforms[i].push_back(variants[i]);
    }
    string best;
    for (auto points : transforms) {
        int minX = points[0].first, minY = points[0].second;
        for (auto [x, y] : points) {
            minX = min(minX, x);
            minY = min(minY, y);
        }
        for (auto& p : points) {
            p.first -= minX;
            p.second -= minY;
        }
        sort(points.begin(), points.end());
        string key;
        for (int i = 0; i < (int)points.size(); i++) {
            if (i) key += "|";
            key += to_string(points[i].first) + ":" + to_string(points[i].second);
        }
        if (best.empty() || key < best) best = key;
    }
    return best;
}

int solve(const vector<vector<int>>& grid) {
    int rows = grid.size(), cols = grid[0].size();
    vector<vector<int>> seen(rows, vector<int>(cols));
    set<string> shapes;
    int dr[4] = {1, -1, 0, 0};
    int dc[4] = {0, 0, 1, -1};
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == 0 || seen[r][c]) continue;
            vector<pair<int, int>> stack{{r, c}}, cells;
            seen[r][c] = 1;
            while (!stack.empty()) {
                auto [x, y] = stack.back();
                stack.pop_back();
                cells.push_back({x - r, y - c});
                for (int k = 0; k < 4; k++) {
                    int nx = x + dr[k], ny = y + dc[k];
                    if (0 <= nx && nx < rows && 0 <= ny && ny < cols && grid[nx][ny] == 1 && !seen[nx][ny]) {
                        seen[nx][ny] = 1;
                        stack.push_back({nx, ny});
                    }
                }
            }
            shapes.insert(canonical(cells));
        }
    }
    return shapes.size();
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; tc++) {
        int rows, cols;
        cin >> rows >> cols;
        vector<vector<int>> grid(rows, vector<int>(cols));
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) cin >> grid[i][j];
        }
        if (tc) cout << "\n\n";
        cout << solve(grid);
    }
}
