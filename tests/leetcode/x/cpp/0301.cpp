#include <algorithm>
#include <iostream>
#include <set>
#include <string>
#include <vector>

using namespace std;

static vector<string> solve(const string& s) {
    int leftRemove = 0, rightRemove = 0;
    for (char ch : s) {
        if (ch == '(') {
            ++leftRemove;
        } else if (ch == ')') {
            if (leftRemove > 0) --leftRemove;
            else ++rightRemove;
        }
    }
    set<string> ans;
    string path;
    function<void(int, int, int, int)> dfs = [&](int i, int left, int right, int balance) {
        if (i == (int)s.size()) {
            if (left == 0 && right == 0 && balance == 0) ans.insert(path);
            return;
        }
        char ch = s[i];
        if (ch == '(') {
            if (left > 0) dfs(i + 1, left - 1, right, balance);
            path.push_back(ch);
            dfs(i + 1, left, right, balance + 1);
            path.pop_back();
        } else if (ch == ')') {
            if (right > 0) dfs(i + 1, left, right - 1, balance);
            if (balance > 0) {
                path.push_back(ch);
                dfs(i + 1, left, right, balance - 1);
                path.pop_back();
            }
        } else {
            path.push_back(ch);
            dfs(i + 1, left, right, balance);
            path.pop_back();
        }
    };
    dfs(0, leftRemove, rightRemove, 0);
    return vector<string>(ans.begin(), ans.end());
}

int main() {
    int t;
    string s;
    if (!getline(cin, s)) return 0;
    t = stoi(s);
    for (int tc = 0; tc < t; ++tc) {
        getline(cin, s);
        vector<string> ans = solve(s);
        if (tc) cout << "\n\n";
        cout << ans.size();
        for (const string& x : ans) cout << '\n' << x;
    }
    return 0;
}
