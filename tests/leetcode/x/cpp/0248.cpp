#include <iostream>
#include <string>
#include <vector>

using namespace std;

static const vector<pair<char, char>> pairs_ = {{'0', '0'}, {'1', '1'}, {'6', '9'}, {'8', '8'}, {'9', '6'}};

static vector<string> build(int n, int m) {
    if (n == 0) return {""};
    if (n == 1) return {"0", "1", "8"};
    vector<string> mids = build(n - 2, m);
    vector<string> res;
    for (const string& mid : mids) {
        for (auto [a, b] : pairs_) {
            if (n == m && a == '0') continue;
            res.push_back(string(1, a) + mid + string(1, b));
        }
    }
    return res;
}

static int count_range(const string& low, const string& high) {
    int ans = 0;
    for (int len = (int)low.size(); len <= (int)high.size(); ++len) {
        for (const string& s : build(len, len)) {
            if (len == (int)low.size() && s < low) continue;
            if (len == (int)high.size() && s > high) continue;
            ans++;
        }
    }
    return ans;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int i = 0; i < t; ++i) {
        string low, high;
        cin >> low >> high;
        if (i) cout << '\n';
        cout << count_range(low, high);
    }
    return 0;
}
