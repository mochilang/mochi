#include <iostream>
#include <vector>
using namespace std;

static int solve(const string& s, const string& t) {
    vector<int> dp(t.size() + 1, 0);
    dp[0] = 1;
    for (char ch : s) {
        for (int j = (int)t.size(); j >= 1; j--) {
            if (ch == t[j - 1]) dp[j] += dp[j - 1];
        }
    }
    return dp[(int)t.size()];
}

int main() {
    int tc;
    if (!(cin >> tc)) return 0;
    string s, t;
    for (int i = 0; i < tc; i++) {
        cin >> s >> t;
        cout << solve(s, t);
        if (i + 1 < tc) cout << '\n';
    }
    return 0;
}
