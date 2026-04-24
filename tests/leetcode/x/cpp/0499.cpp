#include <climits>
#include <iostream>
#include <queue>
#include <string>
#include <tuple>
#include <vector>
using namespace std;

string solve(const vector<vector<int>>& maze, pair<int,int> ball, pair<int,int> hole) {
    int m = maze.size(), n = maze[0].size();
    vector<vector<int>> dist(m, vector<int>(n, INT_MAX));
    vector<vector<string>> path(m, vector<string>(n, ""));
    struct State {
        int d, r, c;
        string p;
        bool operator<(const State& other) const {
            if (d != other.d) return d > other.d;
            return p > other.p;
        }
    };
    priority_queue<State> pq;
    pq.push({0, ball.first, ball.second, ""});
    dist[ball.first][ball.second] = 0;
    path[ball.first][ball.second] = "";
    int dr[4] = {1, 0, 0, -1};
    int dc[4] = {0, -1, 1, 0};
    char ch[4] = {'d', 'l', 'r', 'u'};
    while (!pq.empty()) {
        auto [d, r, c, p] = pq.top(); pq.pop();
        if (r == hole.first && c == hole.second) return p;
        if (d != dist[r][c] || p != path[r][c]) continue;
        for (int k = 0; k < 4; ++k) {
            int nr = r, nc = c, nd = d;
            while (nr + dr[k] >= 0 && nr + dr[k] < m && nc + dc[k] >= 0 && nc + dc[k] < n && maze[nr + dr[k]][nc + dc[k]] == 0) {
                nr += dr[k];
                nc += dc[k];
                ++nd;
                if (nr == hole.first && nc == hole.second) break;
            }
            if (nr == r && nc == c) continue;
            string np = p + ch[k];
            if (nd < dist[nr][nc] || (nd == dist[nr][nc] && (path[nr][nc].empty() || np < path[nr][nc]))) {
                dist[nr][nc] = nd;
                path[nr][nc] = np;
                pq.push({nd, nr, nc, np});
            }
        }
    }
    return "impossible";
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int m, n; cin >> m >> n;
        vector<vector<int>> maze(m, vector<int>(n));
        for (int i = 0; i < m; ++i) for (int j = 0; j < n; ++j) cin >> maze[i][j];
        pair<int,int> ball, hole;
        cin >> ball.first >> ball.second >> hole.first >> hole.second;
        if (tc) cout << "\n\n";
        cout << solve(maze, ball, hole);
    }
}
