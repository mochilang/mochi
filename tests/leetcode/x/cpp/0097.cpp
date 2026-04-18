#include <iostream>
#include <string>
#include <vector>
using namespace std;

static bool solve(const string& s1, const string& s2, const string& s3) {
    int m = (int)s1.size(), n = (int)s2.size();
    if (m + n != (int)s3.size()) return false;
    vector<vector<int>> dp(m + 1, vector<int>(n + 1, 0));
    dp[0][0] = 1;
    for (int i = 0; i <= m; ++i) for (int j = 0; j <= n; ++j) {
        if (i > 0 && dp[i - 1][j] && s1[i - 1] == s3[i + j - 1]) dp[i][j] = 1;
        if (j > 0 && dp[i][j - 1] && s2[j - 1] == s3[i + j - 1]) dp[i][j] = 1;
    }
    return dp[m][n];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    string s1, s2, s3, line;
    if (!getline(cin, line)) return 0;
    t = stoi(line);
    for (int i = 0; i < t; i++) {
        getline(cin, s1); getline(cin, s2); getline(cin, s3);
        cout << (solve(s1, s2, s3) ? "true" : "false");
        if (i + 1 < t) cout << '\n';
    }
    return 0;
}
