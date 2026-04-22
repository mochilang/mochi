#include <stdio.h>
#include <stdlib.h>

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int r, c;
        scanf("%d %d", &r, &c);
        int *rows = malloc((size_t)r * c * sizeof(int));
        int *cols = malloc((size_t)r * c * sizeof(int));
        int rc = 0, cc = 0;
        int *grid = malloc((size_t)r * c * sizeof(int));
        for (int i = 0; i < r; ++i) {
            for (int j = 0; j < c; ++j) {
                scanf("%d", &grid[i * c + j]);
                if (grid[i * c + j] == 1) rows[rc++] = i;
            }
        }
        for (int j = 0; j < c; ++j) {
            for (int i = 0; i < r; ++i) {
                if (grid[i * c + j] == 1) cols[cc++] = j;
            }
        }
        int mr = rows[rc / 2];
        int mc = cols[cc / 2];
        int ans = 0;
        for (int i = 0; i < rc; ++i) ans += rows[i] > mr ? rows[i] - mr : mr - rows[i];
        for (int i = 0; i < cc; ++i) ans += cols[i] > mc ? cols[i] - mc : mc - cols[i];
        if (tc) printf("\n\n");
        printf("%d", ans);
        free(rows);
        free(cols);
        free(grid);
    }
    return 0;
}
