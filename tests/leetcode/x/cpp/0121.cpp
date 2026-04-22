#include <iostream>
#include <vector>
using namespace std;

static int maxProfit(const vector<int>& prices) {
    if (prices.empty()) return 0;
    int minPrice = prices[0], best = 0;
    for (size_t i = 1; i < prices.size(); i++) {
        best = max(best, prices[i] - minPrice);
        minPrice = min(minPrice, prices[i]);
    }
    return best;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n;
        cin >> n;
        vector<int> prices(n);
        for (int i = 0; i < n; i++) cin >> prices[i];
        cout << maxProfit(prices);
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
