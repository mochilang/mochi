#include <stdio.h>
#include <stdlib.h>

static long long count_digit_one(long long n) {
    long long total = 0;
    for (long long m = 1; m <= n; m *= 10) {
        long long high = n / (m * 10);
        long long cur = (n / m) % 10;
        long long low = n % m;
        if (cur == 0) total += high * m;
        else if (cur == 1) total += high * m + low + 1;
        else total += (high + 1) * m;
    }
    return total;
}

int main(void) {
    char line[256];
    if (!fgets(line, sizeof(line), stdin)) return 0;
    int t = atoi(line);
    for (int i = 0; i < t; ++i) {
        if (!fgets(line, sizeof(line), stdin)) break;
        if (i) putchar('\n');
        printf("%lld", count_digit_one(atoll(line)));
    }
    return 0;
}
