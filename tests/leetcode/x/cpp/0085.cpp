#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

static int hist(const vector<int>& h) {
    int best = 0;
    for (int i = 0; i < (int)h.size(); ++i) {
        int mn = h[i];
        for (int j = i; j < (int)h.size(); ++j) {
            mn = min(mn, h[j]);
            best = max(best, mn * (j - i + 1));
        }
    }
    return best;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int rows, cols;
        cin >> rows >> cols;
        vector<int> h(cols, 0);
        int best = 0;
        for (int r = 0; r < rows; ++r) {
            string s;
            cin >> s;
            for (int c = 0; c < cols; ++c) h[c] = s[c] == '1' ? h[c] + 1 : 0;
            best = max(best, hist(h));
        }
        cout << best;
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
