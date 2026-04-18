#include <iostream>
using namespace std;

static bool isPalindrome(int x) {
    if (x < 0) return false;
    int original = x;
    long long rev = 0;
    while (x > 0) {
        rev = rev * 10 + (x % 10);
        x /= 10;
    }
    return rev == original;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    while (t--) {
        int x;
        cin >> x;
        cout << (isPalindrome(x) ? "true" : "false") << "\n";
    }
    return 0;
}
