// Solution for SPOJ TEST - Life, the Universe, and Everything
// https://www.spoj.com/problems/TEST
#include <stdio.h>

int main(void) {
    int n;
    while (scanf("%d", &n) == 1) {
        if (n == 42) {
            break;
        }
        printf("%d\n", n);
    }
    return 0;
}
