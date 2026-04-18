#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static int *vals;
static int *ok;
static int n;
static int best;

static int max2(int a, int b) { return a > b ? a : b; }

static int dfs(int i) {
    if (i >= n || !ok[i]) return 0;
    int left = dfs(2 * i + 1); if (left < 0) left = 0;
    int right = dfs(2 * i + 2); if (right < 0) right = 0;
    int total = vals[i] + left + right;
    if (total > best) best = total;
    return vals[i] + max2(left, right);
}

static int solve(void) {
    best = -1000000000;
    dfs(0);
    return best;
}

int main(void) {
    int tc;
    char buf[64];
    if (scanf("%d", &tc) != 1) return 0;
    for (int t = 0; t < tc; t++) {
        scanf("%d", &n);
        vals = (int*)calloc(n, sizeof(int));
        ok = (int*)calloc(n, sizeof(int));
        for (int i = 0; i < n; i++) {
            scanf("%63s", buf);
            if (strcmp(buf, "null") != 0) {
                ok[i] = 1;
                vals[i] = atoi(buf);
            }
        }
        printf("%d", solve());
        free(vals); free(ok);
        if (t + 1 < tc) putchar('\n');
    }
    return 0;
}
