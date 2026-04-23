#include <iostream>
#include <vector>
using namespace std;

bool isSelfCrossing(vector<int>& x) {
    for (int i = 3; i < (int)x.size(); ++i) {
        if (x[i] >= x[i - 2] && x[i - 1] <= x[i - 3]) return true;
        if (i >= 4 && x[i - 1] == x[i - 3] && x[i] + x[i - 4] >= x[i - 2]) return true;
        if (i >= 5 && x[i - 2] >= x[i - 4] && x[i] + x[i - 4] >= x[i - 2] && x[i - 1] <= x[i - 3] && x[i - 1] + x[i - 5] >= x[i - 3]) return true;
    }
    return false;
}

int main() { ios::sync_with_stdio(false); cin.tie(nullptr); int t; if (!(cin >> t)) return 0; for (int tc = 0; tc < t; ++tc) { int n; cin >> n; vector<int> x(n); for (int &v : x) cin >> v; if (tc) cout << "\n\n"; cout << (isSelfCrossing(x) ? "true" : "false"); } }
