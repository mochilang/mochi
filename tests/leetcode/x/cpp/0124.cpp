#include <iostream>
#include <vector>
#include <string>
using namespace std;

static int dfs(int i, const vector<int>& vals, const vector<int>& ok, int& best) {
    if (i >= (int)vals.size() || !ok[i]) return 0;
    int left = max(0, dfs(2 * i + 1, vals, ok, best));
    int right = max(0, dfs(2 * i + 2, vals, ok, best));
    best = max(best, vals[i] + left + right);
    return vals[i] + max(left, right);
}

static int solve(const vector<int>& vals, const vector<int>& ok) {
    int best = -1000000000;
    dfs(0, vals, ok, best);
    return best;
}

int main() {
    int tc;
    if (!(cin >> tc)) return 0;
    for (int t = 0; t < tc; t++) {
        int n; cin >> n;
        vector<int> vals(n, 0), ok(n, 0);
        for (int i = 0; i < n; i++) {
            string tok; cin >> tok;
            if (tok != "null") { ok[i] = 1; vals[i] = stoi(tok); }
        }
        cout << solve(vals, ok);
        if (t + 1 < tc) cout << '\n';
    }
    return 0;
}
