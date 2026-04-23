#include <cmath>
#include <iostream>
#include <string>
using namespace std;

long long value(long long base, int m, long long limit) {
    long long total = 1, cur = 1;
    for (int i = 0; i < m; ++i) {
        if (cur > limit / base) return limit + 1;
        cur *= base;
        if (total > limit - cur) return limit + 1;
        total += cur;
    }
    return total;
}

string solve(long long n) {
    int maxM = 63 - __builtin_clzll(n);
    for (int m = maxM; m >= 2; --m) {
        long long lo = 2, hi = (long long)(pow((long double)n, 1.0L / m)) + 1;
        while (lo <= hi) {
            long long mid = (lo + hi) / 2;
            long long s = value(mid, m, n);
            if (s == n) return to_string(mid);
            if (s < n) lo = mid + 1;
            else hi = mid - 1;
        }
    }
    return to_string(n - 1);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        long long n;
        cin >> n;
        if (tc) cout << "\n\n";
        cout << solve(n);
    }
    return 0;
}
