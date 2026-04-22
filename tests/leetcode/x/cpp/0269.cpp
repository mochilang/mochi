#include <algorithm>
#include <iostream>
#include <map>
#include <queue>
#include <set>
#include <string>
#include <vector>

using namespace std;

static string solve(const vector<string>& words) {
    set<char> chars;
    for (const string& w : words) for (char c : w) chars.insert(c);
    map<char, set<char>> adj;
    map<char, int> indeg;
    for (char c : chars) indeg[c] = 0;
    for (int i = 0; i + 1 < (int)words.size(); ++i) {
        const string& a = words[i];
        const string& b = words[i + 1];
        int m = min(a.size(), b.size());
        if (a.substr(0, m) == b.substr(0, m) && a.size() > b.size()) return "";
        for (int j = 0; j < m; ++j) {
            if (a[j] != b[j]) {
                if (!adj[a[j]].count(b[j])) {
                    adj[a[j]].insert(b[j]);
                    indeg[b[j]]++;
                }
                break;
            }
        }
    }
    priority_queue<char, vector<char>, greater<char>> pq;
    for (auto [c, d] : indeg) if (d == 0) pq.push(c);
    string out;
    while (!pq.empty()) {
        char c = pq.top();
        pq.pop();
        out.push_back(c);
        for (char nei : adj[c]) {
            if (--indeg[nei] == 0) pq.push(nei);
        }
    }
    return out.size() == chars.size() ? out : "";
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n;
        cin >> n;
        vector<string> words(n);
        for (int i = 0; i < n; ++i) cin >> words[i];
        if (tc) cout << '\n';
        cout << solve(words);
    }
    return 0;
}
