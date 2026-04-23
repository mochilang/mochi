#include <algorithm>
#include <array>
#include <iostream>
#include <string>
#include <unordered_map>
using namespace std;

string shrinkBoard(string s) {
    bool changed = true;
    while (changed) {
        changed = false;
        string out;
        for (int i = 0; i < (int)s.size();) {
            int j = i;
            while (j < (int)s.size() && s[j] == s[i]) ++j;
            if (j - i >= 3) changed = true;
            else out += s.substr(i, j - i);
            i = j;
        }
        s.swap(out);
    }
    return s;
}

int colorId(char c) {
    string colors = "RYBGW";
    return (int)colors.find(c);
}

string encodeState(const string& board, const array<int, 5>& hand) {
    return board + "|" + to_string(hand[0]) + "," + to_string(hand[1]) + "," + to_string(hand[2]) + "," + to_string(hand[3]) + "," + to_string(hand[4]);
}

int dfs(const string& board, array<int, 5>& hand, unordered_map<string, int>& memo) {
    string cur = shrinkBoard(board);
    if (cur.empty()) return 0;
    string key = encodeState(cur, hand);
    if (memo.count(key)) return memo[key];
    const int INF = 1e9;
    int best = INF;
    for (int i = 0; i < (int)cur.size();) {
        int j = i;
        while (j < (int)cur.size() && cur[j] == cur[i]) ++j;
        int need = 3 - (j - i);
        int id = colorId(cur[i]);
        if (hand[id] >= max(need, 0)) {
            int used = max(need, 0);
            hand[id] -= used;
            int sub = dfs(cur.substr(0, i) + cur.substr(j), hand, memo);
            if (sub != INF) best = min(best, used + sub);
            hand[id] += used;
        }
        i = j;
    }
    return memo[key] = best;
}

int solve(const string& board, const string& handStr) {
    array<int, 5> hand{};
    for (char c : handStr) ++hand[colorId(c)];
    unordered_map<string, int> memo;
    int ans = dfs(board, hand, memo);
    return ans >= (int)1e9 ? -1 : ans;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        string board, hand;
        cin >> board >> hand;
        if (tc) cout << "\n\n";
        cout << solve(board, hand);
    }
    return 0;
}
