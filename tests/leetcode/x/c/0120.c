#include <stdio.h>

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int rows;
        scanf("%d", &rows);
        int tri[256][256], dp[256];
        for (int r = 0; r < rows; r++)
            for (int j = 0; j <= r; j++)
                scanf("%d", &tri[r][j]);
        for (int j = 0; j < rows; j++) dp[j] = tri[rows - 1][j];
        for (int i = rows - 2; i >= 0; i--)
            for (int j = 0; j <= i; j++)
                dp[j] = tri[i][j] + (dp[j] < dp[j + 1] ? dp[j] : dp[j + 1]);
        printf("%d", dp[0]);
        if (tc + 1 < t) printf("\n");
    }
    return 0;
}
