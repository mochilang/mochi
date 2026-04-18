#include <stdio.h>
#include <stdlib.h>

int cmp(const void *a, const void *b) {
    int x = *(const int*)a, y = *(const int*)b;
    return (x > y) - (x < y);
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int k, cap = 16, len = 0;
        scanf("%d", &k);
        int *vals = malloc(sizeof(int) * cap);
        for (int i = 0; i < k; ++i) {
            int n; scanf("%d", &n);
            for (int j = 0; j < n; ++j) {
                int x; scanf("%d", &x);
                if (len == cap) { cap *= 2; vals = realloc(vals, sizeof(int) * cap); }
                vals[len++] = x;
            }
        }
        qsort(vals, len, sizeof(int), cmp);
        printf("[");
        for (int i = 0; i < len; ++i) { if (i) printf(","); printf("%d", vals[i]); }
        printf("]");
        if (tc + 1 < t) printf("\n");
        free(vals);
    }
    return 0;
}
