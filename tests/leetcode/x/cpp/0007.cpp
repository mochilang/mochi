#include <iostream>
#include <climits>
using namespace std;

static int reverseInt(int x) {
    int ans = 0;
    while (x != 0) {
        int digit = x % 10;
        x /= 10;
        if (ans > INT_MAX / 10 || (ans == INT_MAX / 10 && digit > 7)) return 0;
        if (ans < INT_MIN / 10 || (ans == INT_MIN / 10 && digit < -8)) return 0;
        ans = ans * 10 + digit;
    }
    return ans;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int i = 0; i < t; i++) {
        int x;
        cin >> x;
        cout << reverseInt(x);
        if (i + 1 < t) cout << '\n';
    }
    return 0;
}
