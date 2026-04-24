#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

static int solve(const std::string& ring, const std::string& key) {
    int n = static_cast<int>(ring.size());
    std::unordered_map<char, std::vector<int>> positions;
    for (int i = 0; i < n; ++i) {
        positions[ring[i]].push_back(i);
    }
    std::unordered_map<int, int> dp;
    dp[0] = 0;
    for (char ch : key) {
        std::unordered_map<int, int> next;
        for (int j : positions[ch]) {
            int best = 1e9;
            for (const auto& [i, cost] : dp) {
                int diff = std::abs(i - j);
                int step = std::min(diff, n - diff);
                best = std::min(best, cost + step);
            }
            next[j] = best;
        }
        dp = std::move(next);
    }
    int ans = 1e9;
    for (const auto& [_, cost] : dp) {
        ans = std::min(ans, cost);
    }
    return ans + static_cast<int>(key.size());
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int t;
    if (!(std::cin >> t)) {
        return 0;
    }
    for (int tc = 0; tc < t; ++tc) {
        std::string ring, key;
        std::cin >> ring >> key;
        if (tc) {
            std::cout << "\n\n";
        }
        std::cout << solve(ring, key);
    }
    return 0;
}
