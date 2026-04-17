#include <iostream>
#include <vector>
#include <string>
using namespace std;

static string lcp(vector<string> strs) {
    string prefix = strs[0];
    while (true) {
        bool ok = true;
        for (const auto& s : strs) if (s.rfind(prefix, 0) != 0) { ok = false; break; }
        if (ok) return prefix;
        prefix.pop_back();
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t, n;
    if (!(cin >> t)) return 0;
    while (t--) {
        cin >> n;
        vector<string> strs(n);
        for (int i = 0; i < n; i++) cin >> strs[i];
        cout << "\"" << lcp(strs) << "\"\n";
    }
    return 0;
}
