#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>

using namespace std;

static vector<int> solve(const vector<int>& values, double target, int k) {
    int right = lower_bound(values.begin(), values.end(), target) - values.begin();
    int left = right - 1;
    vector<int> ans;
    while ((int)ans.size() < k) {
        if (left < 0) ans.push_back(values[right++]);
        else if (right >= (int)values.size()) ans.push_back(values[left--]);
        else if (fabs(values[left] - target) <= fabs(values[right] - target)) ans.push_back(values[left--]);
        else ans.push_back(values[right++]);
    }
    return ans;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n;
        cin >> n;
        vector<int> values(n);
        for (int i = 0; i < n; ++i) cin >> values[i];
        double target;
        int k;
        cin >> target >> k;
        vector<int> ans = solve(values, target, k);
        if (tc) cout << "\n\n";
        cout << ans.size();
        for (int x : ans) cout << '\n' << x;
    }
    return 0;
}
