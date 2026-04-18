#include <stdio.h>

static int max_profit(int *prices, int n) {
    int best = 0;
    for (int i = 1; i < n; i++) {
        if (prices[i] > prices[i - 1]) best += prices[i] - prices[i - 1];
    }
    return best;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n;
        int prices[100005];
        scanf("%d", &n);
        for (int i = 0; i < n; i++) scanf("%d", &prices[i]);
        printf("%d", max_profit(prices, n));
        if (tc + 1 < t) putchar('\n');
    }
    return 0;
}
