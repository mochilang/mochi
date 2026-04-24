#include <algorithm>
#include <iostream>
#include <tuple>
#include <unordered_map>
#include <vector>

struct StateHash {
    std::size_t operator()(const std::tuple<int, int, int>& s) const noexcept {
        auto [l, r, k] = s;
        return (static_cast<std::size_t>(l) * 1315423911u) ^ (static_cast<std::size_t>(r) << 10) ^ static_cast<std::size_t>(k);
    }
};

static int solve(const std::vector<int>& boxes) {
    std::unordered_map<std::tuple<int, int, int>, int, StateHash> memo;
    auto dp = [&](auto&& self, int l, int r, int k) -> int {
        if (l > r) {
            return 0;
        }
        while (l < r && boxes[r] == boxes[r - 1]) {
            --r;
            ++k;
        }
        auto key = std::make_tuple(l, r, k);
        auto it = memo.find(key);
        if (it != memo.end()) {
            return it->second;
        }
        int best = self(self, l, r - 1, 0) + (k + 1) * (k + 1);
        for (int i = l; i < r; ++i) {
            if (boxes[i] == boxes[r]) {
                best = std::max(best, self(self, l, i, k + 1) + self(self, i + 1, r - 1, 0));
            }
        }
        memo[key] = best;
        return best;
    };
    return dp(dp, 0, static_cast<int>(boxes.size()) - 1, 0);
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int t;
    if (!(std::cin >> t)) {
        return 0;
    }
    for (int tc = 0; tc < t; ++tc) {
        int n;
        std::cin >> n;
        std::vector<int> boxes(n);
        for (int i = 0; i < n; ++i) {
            std::cin >> boxes[i];
        }
        if (tc) {
            std::cout << "\n\n";
        }
        std::cout << solve(boxes);
    }
    return 0;
}
