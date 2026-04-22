#include <iostream>
#include <vector>

using namespace std;

int minTotalDistance(const vector<vector<int>>& grid) {
    vector<int> rows;
    vector<int> cols;
    for (int i = 0; i < (int)grid.size(); ++i) {
        for (int j = 0; j < (int)grid[i].size(); ++j) {
            if (grid[i][j]) rows.push_back(i);
        }
    }
    for (int j = 0; j < (int)grid[0].size(); ++j) {
        for (int i = 0; i < (int)grid.size(); ++i) {
            if (grid[i][j]) cols.push_back(j);
        }
    }
    int mr = rows[rows.size() / 2];
    int mc = cols[cols.size() / 2];
    int ans = 0;
    for (int r : rows) ans += r > mr ? r - mr : mr - r;
    for (int c : cols) ans += c > mc ? c - mc : mc - c;
    return ans;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int r, c;
        cin >> r >> c;
        vector<vector<int>> grid(r, vector<int>(c));
        for (int i = 0; i < r; ++i) {
            for (int j = 0; j < c; ++j) cin >> grid[i][j];
        }
        if (tc) cout << "\n\n";
        cout << minTotalDistance(grid);
    }
    return 0;
}
