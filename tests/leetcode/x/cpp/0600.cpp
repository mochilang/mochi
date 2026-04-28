#include <iostream>
#include <string>
#include <vector>

static int solve(int n) {
    std::vector<int> f(32);
    f[0] = 1;
    f[1] = 2;
    for (int i = 2; i < 32; ++i) {
        f[i] = f[i - 1] + f[i - 2];
    }

    int ans = 0;
    int prev = 0;
    for (int i = 30; i >= 0; --i) {
        if (n & (1 << i)) {
            ans += f[i];
            if (prev == 1) return ans;
            prev = 1;
        } else {
            prev = 0;
        }
    }
    return ans + 1;
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);
    int t;
    if (!(std::cin >> t)) return 0;
    for (int i = 0; i < t; ++i) {
        int n;
        std::cin >> n;
        if (i) std::cout << "\n\n";
        std::cout << solve(n);
    }
    return 0;
}
