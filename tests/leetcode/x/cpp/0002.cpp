#include <iostream>
#include <string>
#include <vector>
using namespace std;

static vector<int> addLists(const vector<int>& a, const vector<int>& b) {
    vector<int> out;
    int i = 0, j = 0, carry = 0;
    while (i < (int)a.size() || j < (int)b.size() || carry) {
        int sum = carry;
        if (i < (int)a.size()) sum += a[i++];
        if (j < (int)b.size()) sum += b[j++];
        out.push_back(sum % 10);
        carry = sum / 10;
    }
    return out;
}

static string format(const vector<int>& a) {
    string s = "[";
    for (int i = 0; i < (int)a.size(); i++) {
        if (i) s += ",";
        s += to_string(a[i]);
    }
    return s + "]";
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if (!(cin >> t)) return 0;
    while (t--) {
        int n, m; cin >> n;
        vector<int> a(n); for (int i = 0; i < n; i++) cin >> a[i];
        cin >> m;
        vector<int> b(m); for (int i = 0; i < m; i++) cin >> b[i];
        cout << format(addLists(a, b)) << "\n";
    }
    return 0;
}
