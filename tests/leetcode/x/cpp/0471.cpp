#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

string better(const string& a, const string& b) {
    if (a.size() != b.size()) return a.size() < b.size() ? a : b;
    return a < b ? a : b;
}

int repeat_len(const string& s) {
    string doubled = s + s;
    size_t pos = doubled.find(s, 1);
    if (pos != string::npos && pos < s.size() && s.size() % pos == 0) return (int)pos;
    return (int)s.size();
}

string encode(const string& s) {
    int n = (int)s.size();
    vector<vector<string>> dp(n, vector<string>(n));
    for (int len = 1; len <= n; ++len) {
        for (int i = 0; i + len <= n; ++i) {
            int j = i + len - 1;
            string sub = s.substr(i, len);
            string best = sub;
            for (int k = i; k < j; ++k) best = better(best, dp[i][k] + dp[k + 1][j]);
            int part = repeat_len(sub);
            if (part < len) {
                string cand = to_string(len / part) + "[" + dp[i][i + part - 1] + "]";
                best = better(best, cand);
            }
            dp[i][j] = best;
        }
    }
    return dp[0][n - 1];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        string s;
        cin >> s;
        if (tc) cout << "\n\n";
        cout << encode(s);
    }
    return 0;
}
