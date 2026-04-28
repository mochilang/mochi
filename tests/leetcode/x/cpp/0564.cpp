#include <cmath>
#include <cstdint>
#include <iostream>
#include <set>
#include <string>

static long long pow10(int e) {
    long long v = 1;
    while (e-- > 0) v *= 10;
    return v;
}

static long long make_pal(const std::string& prefix, bool odd) {
    std::string s = prefix;
    for (int i = static_cast<int>(prefix.size()) - 1 - (odd ? 1 : 0); i >= 0; --i) {
        s.push_back(prefix[i]);
    }
    return std::stoll(s);
}

static std::string solve(const std::string& n) {
    int m = static_cast<int>(n.size());
    long long x = std::stoll(n);
    std::set<long long> cands;
    cands.insert(pow10(m - 1) - 1);
    cands.insert(pow10(m) + 1);
    long long prefix = std::stoll(n.substr(0, (m + 1) / 2));
    for (long long d : {-1LL, 0LL, 1LL}) {
        cands.insert(make_pal(std::to_string(prefix + d), m % 2 == 1));
    }
    cands.erase(x);
    long long best = -1;
    for (long long cand : cands) {
        if (cand < 0) continue;
        if (best == -1 || std::llabs(cand - x) < std::llabs(best - x) ||
            (std::llabs(cand - x) == std::llabs(best - x) && cand < best)) {
            best = cand;
        }
    }
    return std::to_string(best);
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);
    int t;
    if (!(std::cin >> t)) return 0;
    for (int i = 0; i < t; ++i) {
        std::string n;
        std::cin >> n;
        if (i) std::cout << "\n\n";
        std::cout << solve(n);
    }
    return 0;
}
