#include <iostream>
#include <set>
#include <string>
#include <vector>
using namespace std;

struct Robot {
    vector<string> grid;
    int r, c, dir;
    set<pair<int, int>> cleaned;
    static constexpr int dr[4] = {-1, 0, 1, 0};
    static constexpr int dc[4] = {0, 1, 0, -1};

    Robot(vector<string> g, int sr, int sc) : grid(std::move(g)), r(sr), c(sc), dir(0) {}

    bool move() {
        int nr = r + dr[dir], nc = c + dc[dir];
        if (nr < 0 || nr >= (int)grid.size() || nc < 0 || nc >= (int)grid[0].size() || grid[nr][nc] != '1') return false;
        r = nr;
        c = nc;
        return true;
    }
    void turnRight() { dir = (dir + 1) % 4; }
    void turnLeft() { dir = (dir + 3) % 4; }
    void clean() { cleaned.insert({r, c}); }
};

void goBack(Robot& robot) {
    robot.turnRight();
    robot.turnRight();
    robot.move();
    robot.turnRight();
    robot.turnRight();
}

void dfs(Robot& robot, int x, int y, int dir, set<pair<int, int>>& vis) {
    vis.insert({x, y});
    robot.clean();
    static constexpr int dr[4] = {-1, 0, 1, 0};
    static constexpr int dc[4] = {0, 1, 0, -1};
    for (int i = 0; i < 4; ++i) {
        int nd = (dir + i) % 4;
        int nx = x + dr[nd], ny = y + dc[nd];
        if (!vis.count({nx, ny}) && robot.move()) {
            dfs(robot, nx, ny, nd, vis);
            goBack(robot);
        }
        robot.turnRight();
    }
}

int solve(const vector<string>& grid, int sr, int sc) {
    Robot robot(grid, sr, sc);
    set<pair<int, int>> vis;
    dfs(robot, 0, 0, 0, vis);
    return (int)robot.cleaned.size();
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n, m, sr, sc;
        cin >> n >> m >> sr >> sc;
        vector<string> grid(n);
        for (int i = 0; i < n; ++i) cin >> grid[i];
        if (tc) cout << "\n\n";
        cout << solve(grid, sr, sc);
    }
    return 0;
}
