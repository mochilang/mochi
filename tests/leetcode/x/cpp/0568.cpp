#include <iostream>
#include <vector>

static int solve(const std::vector<std::vector<int>>& flights, const std::vector<std::vector<int>>& days) {
    int n = static_cast<int>(flights.size());
    int weeks = static_cast<int>(days[0].size());
    const int neg = -1000000000;
    std::vector<int> dp(n, neg);
    dp[0] = 0;
    for (int week = 0; week < weeks; ++week) {
        std::vector<int> ndp(n, neg);
        for (int city = 0; city < n; ++city) {
            if (dp[city] == neg) continue;
            for (int nxt = 0; nxt < n; ++nxt) {
                if (city == nxt || flights[city][nxt]) {
                    ndp[nxt] = std::max(ndp[nxt], dp[city] + days[nxt][week]);
                }
            }
        }
        dp = std::move(ndp);
    }
    int best = dp[0];
    for (int v : dp) best = std::max(best, v);
    return best;
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);
    int t;
    if (!(std::cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n, w;
        std::cin >> n >> w;
        std::vector<std::vector<int>> flights(n, std::vector<int>(n));
        std::vector<std::vector<int>> days(n, std::vector<int>(w));
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < n; ++j)
                std::cin >> flights[i][j];
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < w; ++j)
                std::cin >> days[i][j];
        if (tc) std::cout << "\n\n";
        std::cout << solve(flights, days);
    }
    return 0;
}
