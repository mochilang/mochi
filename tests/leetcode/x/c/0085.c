#include <stdio.h>

static int hist(int *h, int n) {
    int best = 0;
    for (int i = 0; i < n; i++) {
        int mn = h[i];
        for (int j = i; j < n; j++) {
            if (h[j] < mn) mn = h[j];
            int area = mn * (j - i + 1);
            if (area > best) best = area;
        }
    }
    return best;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int rows, cols;
        scanf("%d%d", &rows, &cols);
        int h[256] = {0};
        int best = 0;
        char row[512];
        for (int r = 0; r < rows; r++) {
            scanf("%s", row);
            for (int c = 0; c < cols; c++) h[c] = (row[c] == '1') ? h[c] + 1 : 0;
            int area = hist(h, cols);
            if (area > best) best = area;
        }
        printf("%d", best);
        if (tc + 1 < t) printf("\n");
    }
    return 0;
}
