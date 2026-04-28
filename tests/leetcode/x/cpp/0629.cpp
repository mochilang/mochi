#include <iostream>
#include <vector>

static constexpr int MOD = 1000000007;

static int solve(int n, int k) {
    std::vector<int> dp(k + 1);
    dp[0] = 1;
    for (int num = 1; num <= n; ++num) {
        std::vector<int> ndp(k + 1);
        int window = 0;
        for (int inv = 0; inv <= k; ++inv) {
            window += dp[inv];
            if (window >= MOD) window -= MOD;
            if (inv >= num) {
                window -= dp[inv - num];
                if (window < 0) window += MOD;
            }
            ndp[inv] = window;
        }
        dp = std::move(ndp);
    }
    return dp[k];
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);
    int t;
    if (!(std::cin >> t)) return 0;
    for (int i = 0; i < t; ++i) {
        int n, k;
        std::cin >> n >> k;
        if (i) std::cout << "\n\n";
        std::cout << solve(n, k);
    }
    return 0;
}
