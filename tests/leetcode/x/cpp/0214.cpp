#include <iostream>
#include <string>
#include <vector>
using namespace std;

static string solve(const string& s) {
    string rev(s.rbegin(), s.rend());
    string combined = s + "#" + rev;
    vector<int> pi(combined.size(), 0);
    for (int i = 1; i < (int)combined.size(); ++i) {
        int j = pi[i - 1];
        while (j > 0 && combined[i] != combined[j]) j = pi[j - 1];
        if (combined[i] == combined[j]) ++j;
        pi[i] = j;
    }
    int keep = pi.empty() ? 0 : pi.back();
    return rev.substr(0, s.size() - keep) + s;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    string s;
    getline(cin, s);
    for (int tc = 0; tc < t; ++tc) {
        getline(cin, s);
        cout << solve(s);
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
