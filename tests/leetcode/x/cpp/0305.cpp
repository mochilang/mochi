#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

static vector<int> solve(int m, int n, const vector<pair<int, int>>& positions) {
    unordered_map<int, int> parent;
    unordered_map<int, int> rankv;
    int count = 0;
    vector<int> ans;

    auto find = [&](int x) {
        while (parent[x] != x) {
            parent[x] = parent[parent[x]];
            x = parent[x];
        }
        return x;
    };

    auto unite = [&](int a, int b) {
        int ra = find(a), rb = find(b);
        if (ra == rb) return false;
        if (rankv[ra] < rankv[rb]) swap(ra, rb);
        parent[rb] = ra;
        if (rankv[ra] == rankv[rb]) rankv[ra]++;
        return true;
    };

    for (auto [r, c] : positions) {
        int idx = r * n + c;
        if (parent.count(idx)) {
            ans.push_back(count);
            continue;
        }
        parent[idx] = idx;
        rankv[idx] = 0;
        count++;
        const int dr[4] = {1, -1, 0, 0};
        const int dc[4] = {0, 0, 1, -1};
        for (int k = 0; k < 4; ++k) {
            int nr = r + dr[k], nc = c + dc[k];
            if (nr >= 0 && nr < m && nc >= 0 && nc < n) {
                int nei = nr * n + nc;
                if (parent.count(nei) && unite(idx, nei)) count--;
            }
        }
        ans.push_back(count);
    }
    return ans;
}

static string fmtList(const vector<int>& a) {
    string out = "[";
    for (int i = 0; i < (int)a.size(); ++i) {
        if (i) out += ",";
        out += to_string(a[i]);
    }
    out += "]";
    return out;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int m, n, k;
        cin >> m >> n >> k;
        vector<pair<int, int>> positions(k);
        for (int i = 0; i < k; ++i) cin >> positions[i].first >> positions[i].second;
        if (tc) cout << "\n\n";
        cout << fmtList(solve(m, n, positions));
    }
    return 0;
}
