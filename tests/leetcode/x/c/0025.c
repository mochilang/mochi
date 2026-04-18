#include <stdio.h>
#include <stdlib.h>
int main(void) {
    int t; if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n, k; scanf("%d", &n);
        int *arr = malloc(sizeof(int) * (n > 0 ? n : 1));
        for (int i = 0; i < n; ++i) scanf("%d", &arr[i]);
        scanf("%d", &k);
        for (int i = 0; i + k <= n; i += k) {
            for (int l = i, r = i + k - 1; l < r; ++l, --r) { int tmp = arr[l]; arr[l] = arr[r]; arr[r] = tmp; }
        }
        printf("[");
        for (int i = 0; i < n; ++i) { if (i) printf(","); printf("%d", arr[i]); }
        printf("]");
        if (tc + 1 < t) printf("\n");
        free(arr);
    }
    return 0;
}
