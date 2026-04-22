#include <algorithm>
#include <iostream>
#include <queue>
#include <tuple>
#include <vector>
using namespace std;

static vector<pair<int, int>> solve(const vector<array<int, 3>>& buildings) {
    vector<tuple<int, int, int>> events;
    for (auto &b : buildings) {
        events.push_back({b[0], -b[2], b[1]});
        events.push_back({b[1], 0, 0});
    }
    sort(events.begin(), events.end());
    priority_queue<pair<int, int>> pq;
    pq.push({0, 1 << 30});
    vector<pair<int, int>> ans;
    for (int i = 0; i < (int)events.size();) {
        int x = get<0>(events[i]);
        while (i < (int)events.size() && get<0>(events[i]) == x) {
            if (get<1>(events[i]) != 0) pq.push({-get<1>(events[i]), get<2>(events[i])});
            ++i;
        }
        while (!pq.empty() && pq.top().second <= x) pq.pop();
        int height = pq.top().first;
        if (ans.empty() || ans.back().second != height) ans.push_back({x, height});
    }
    return ans;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n;
        cin >> n;
        vector<array<int, 3>> buildings(n);
        for (int i = 0; i < n; ++i) cin >> buildings[i][0] >> buildings[i][1] >> buildings[i][2];
        auto ans = solve(buildings);
        cout << ans.size();
        for (auto &p : ans) cout << '\n' << p.first << ' ' << p.second;
        if (tc + 1 < t) cout << "\n\n";
    }
    return 0;
}
