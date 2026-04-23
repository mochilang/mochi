#include <iostream>
#include <string>
using namespace std;

long long make_pal(long long left) {
    string s = to_string(left);
    string r = s;
    reverse(r.begin(), r.end());
    return stoll(s + r);
}

int solve(int n) {
    if (n == 1) return 9;
    long long upper = 1;
    for (int i = 0; i < n; ++i) upper *= 10;
    long long lower = upper / 10;
    upper -= 1;
    for (long long left = upper; left >= lower; --left) {
        long long pal = make_pal(left);
        for (long long x = upper; x * x >= pal; --x) {
            if (pal % x == 0) {
                long long y = pal / x;
                if (y >= lower && y <= upper) return (int)(pal % 1337);
            }
        }
    }
    return -1;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n;
        cin >> n;
        if (tc) cout << "\n\n";
        cout << solve(n);
    }
    return 0;
}
