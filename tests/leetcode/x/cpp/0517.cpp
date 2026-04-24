#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

static int solve(const std::vector<int>& machines) {
    int total = 0;
    for (int x : machines) {
        total += x;
    }
    int n = static_cast<int>(machines.size());
    if (total % n != 0) {
        return -1;
    }
    int target = total / n;
    int flow = 0;
    int ans = 0;
    for (int x : machines) {
        int diff = x - target;
        flow += diff;
        if (std::abs(flow) > ans) ans = std::abs(flow);
        if (diff > ans) ans = diff;
    }
    return ans;
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
        std::vector<int> machines(n);
        for (int i = 0; i < n; ++i) {
            std::cin >> machines[i];
        }
        if (tc) std::cout << "\n\n";
        std::cout << solve(machines);
    }
    return 0;
}
