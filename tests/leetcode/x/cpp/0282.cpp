#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

static vector<string> solve(const string& num, long long target) {
    vector<string> ans;
    function<void(int, string, long long, long long)> dfs = [&](int i, string expr, long long value, long long last) {
        if (i == (int)num.size()) {
            if (value == target) ans.push_back(expr);
            return;
        }
        for (int j = i; j < (int)num.size(); ++j) {
            if (j > i && num[i] == '0') break;
            string s = num.substr(i, j - i + 1);
            long long n = stoll(s);
            if (i == 0) {
                dfs(j + 1, s, n, n);
            } else {
                dfs(j + 1, expr + "+" + s, value + n, n);
                dfs(j + 1, expr + "-" + s, value - n, -n);
                dfs(j + 1, expr + "*" + s, value - last + last * n, last * n);
            }
        }
    };
    dfs(0, "", 0, 0);
    sort(ans.begin(), ans.end());
    return ans;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        string num;
        long long target;
        cin >> num >> target;
        vector<string> ans = solve(num, target);
        if (tc) cout << "\n\n";
        cout << ans.size();
        for (const string& s : ans) cout << '\n' << s;
    }
    return 0;
}
