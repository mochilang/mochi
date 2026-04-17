#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;
int main() {
    int t; if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int k; cin >> k; vector<int> vals;
        for (int i = 0; i < k; ++i) {
            int n; cin >> n;
            for (int j = 0; j < n; ++j) { int x; cin >> x; vals.push_back(x); }
        }
        sort(vals.begin(), vals.end());
        cout << '[';
        for (size_t i = 0; i < vals.size(); ++i) { if (i) cout << ','; cout << vals[i]; }
        cout << ']';
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
