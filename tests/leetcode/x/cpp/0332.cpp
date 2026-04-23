#include <algorithm>
#include <functional>
#include <iostream>
#include <map>
#include <string>
#include <vector>
using namespace std;

vector<string> findItinerary(vector<pair<string,string>>& tickets) {
    map<string, vector<string>> graph;
    for (auto &t : tickets) graph[t.first].push_back(t.second);
    for (auto &kv : graph) sort(kv.second.rbegin(), kv.second.rend());
    vector<string> route;
    function<void(const string&)> visit = [&](const string& a) {
        auto &dests = graph[a];
        while (!dests.empty()) { string next = dests.back(); dests.pop_back(); visit(next); }
        route.push_back(a);
    };
    visit("JFK");
    reverse(route.begin(), route.end());
    return route;
}

string fmt(const vector<string>& r) { string s = "["; for (int i = 0; i < (int)r.size(); ++i) { if (i) s += ","; s += "\"" + r[i] + "\""; } return s + "]"; }
int main() { ios::sync_with_stdio(false); cin.tie(nullptr); int t; if (!(cin >> t)) return 0; for (int tc = 0; tc < t; ++tc) { int m; cin >> m; vector<pair<string,string>> tickets(m); for (auto &p : tickets) cin >> p.first >> p.second; if (tc) cout << "\n\n"; cout << fmt(findItinerary(tickets)); } }
