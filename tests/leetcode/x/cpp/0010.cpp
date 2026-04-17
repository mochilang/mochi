#include <iostream>
#include <string>
using namespace std;

bool matchAt(const string& s, const string& p, int i, int j) {
    if (j == (int)p.size()) return i == (int)s.size();
    bool first = i < (int)s.size() && (p[j] == '.' || s[i] == p[j]);
    if (j + 1 < (int)p.size() && p[j + 1] == '*') {
        return matchAt(s, p, i, j + 2) || (first && matchAt(s, p, i + 1, j));
    }
    return first && matchAt(s, p, i + 1, j + 1);
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    string s, p;
    for (int tc = 0; tc < t; ++tc) {
        cin >> s >> p;
        cout << (matchAt(s, p, 0, 0) ? "true" : "false");
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
