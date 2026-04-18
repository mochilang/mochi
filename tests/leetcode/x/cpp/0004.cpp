#include <iostream>
#include <vector>
#include <iomanip>
using namespace std;

double median(const vector<int>& a, const vector<int>& b) {
    vector<int> m; m.reserve(a.size() + b.size());
    size_t i = 0, j = 0;
    while (i < a.size() && j < b.size()) {
        if (a[i] <= b[j]) m.push_back(a[i++]); else m.push_back(b[j++]);
    }
    while (i < a.size()) m.push_back(a[i++]);
    while (j < b.size()) m.push_back(b[j++]);
    int n = (int)m.size();
    if (n % 2) return m[n / 2];
    return (m[n / 2 - 1] + m[n / 2]) / 2.0;
}

int main() {
    int t; if (!(cin >> t)) return 0;
    cout << fixed << setprecision(1);
    for (int tc = 0; tc < t; tc++) {
        int n, m; cin >> n; vector<int> a(n); for (int i=0;i<n;i++) cin >> a[i]; cin >> m; vector<int> b(m); for (int i=0;i<m;i++) cin >> b[i];
        cout << median(a,b); if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
