#include <stdio.h>

static int max_profit(int *prices, int n) {
    int buy1 = -1000000000, sell1 = 0, buy2 = -1000000000, sell2 = 0;
    for (int i = 0; i < n; i++) {
        int p = prices[i];
        if (-p > buy1) buy1 = -p;
        if (buy1 + p > sell1) sell1 = buy1 + p;
        if (sell1 - p > buy2) buy2 = sell1 - p;
        if (buy2 + p > sell2) sell2 = buy2 + p;
    }
    return sell2;
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
