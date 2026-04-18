#include <algorithm>
#include <iostream>
#include <vector>
using namespace std;

static int solve(const vector<int>& a) {
    int best = 0;
    for (int i = 0; i < (int)a.size(); ++i) {
        int mn = a[i];
        for (int j = i; j < (int)a.size(); ++j) {
            mn = min(mn, a[j]);
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
        int n;
        cin >> n;
        vector<int> a(n);
        for (int i = 0; i < n; ++i) cin >> a[i];
        cout << solve(a);
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
