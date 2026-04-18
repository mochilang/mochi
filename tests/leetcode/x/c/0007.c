#include <stdio.h>
#include <limits.h>

static int reverse_int(int x) {
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

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int i = 0; i < t; i++) {
        int x;
        if (scanf("%d", &x) != 1) x = 0;
        printf("%d", reverse_int(x));
        if (i + 1 < t) putchar('\n');
    }
    return 0;
}
