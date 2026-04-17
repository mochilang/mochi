#include <stdio.h>
#include <stdbool.h>

static bool is_palindrome(int x) {
    if (x < 0) return false;
    int original = x;
    long long rev = 0;
    while (x > 0) {
        rev = rev * 10 + (x % 10);
        x /= 10;
    }
    return rev == original;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int i = 0; i < t; i++) {
        int x;
        scanf("%d", &x);
        puts(is_palindrome(x) ? "true" : "false");
    }
    return 0;
}
