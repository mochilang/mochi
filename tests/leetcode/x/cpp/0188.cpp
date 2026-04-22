#include <iostream>
#include <vector>
using namespace std;

static int solve(int k, const vector<int>& prices) {
    int n = (int)prices.size();
    if (k >= n / 2) {
        int best = 0;
        for (int i = 1; i < n; ++i) if (prices[i] > prices[i - 1]) best += prices[i] - prices[i - 1];
        return best;
    }
    const long long negInf = -(1LL << 60);
    vector<long long> buy(k + 1, negInf), sell(k + 1, 0);
    for (int price : prices) {
        for (int t = 1; t <= k; ++t) {
            buy[t] = max(buy[t], sell[t - 1] - price);
            sell[t] = max(sell[t], buy[t] + price);
        }
    }
    return (int)sell[k];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int k, n;
        cin >> k >> n;
        vector<int> prices(n);
        for (int i = 0; i < n; ++i) cin >> prices[i];
        cout << solve(k, prices);
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
