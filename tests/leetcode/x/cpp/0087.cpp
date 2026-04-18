#include <iostream>
#include <string>
#include <unordered_map>
using namespace std;

string s1, s2;
unordered_map<string, bool> memo;

bool dfs(int i1, int i2, int len) {
    string key = to_string(i1) + "," + to_string(i2) + "," + to_string(len);
    if (memo.count(key)) return memo[key];
    string a = s1.substr(i1, len), b = s2.substr(i2, len);
    if (a == b) return memo[key] = true;
    int cnt[26] = {0};
    for (int i = 0; i < len; i++) {
        cnt[a[i] - 'a']++;
        cnt[b[i] - 'a']--;
    }
    for (int v : cnt) if (v != 0) return memo[key] = false;
    for (int k = 1; k < len; k++) {
        if ((dfs(i1, i2, k) && dfs(i1 + k, i2 + k, len - k)) ||
            (dfs(i1, i2 + len - k, k) && dfs(i1 + k, i2, len - k))) {
            return memo[key] = true;
        }
    }
    return memo[key] = false;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    string line;
    if (!getline(cin, line)) return 0;
    t = stoi(line);
    for (int i = 0; i < t; i++) {
        getline(cin, s1);
        getline(cin, s2);
        memo.clear();
        cout << (dfs(0, 0, (int)s1.size()) ? "true" : "false");
        if (i + 1 < t) cout << '\n';
    }
    return 0;
}
