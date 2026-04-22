#include <stdio.h>

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int rows, cols;
        scanf("%d %d", &rows, &cols);
        int dungeon[256][256], dp[257];
        for (int i = 0; i < rows; i++)
            for (int j = 0; j < cols; j++)
                scanf("%d", &dungeon[i][j]);
        for (int j = 0; j <= cols; j++) dp[j] = 1000000000;
        dp[cols - 1] = 1;
        for (int i = rows - 1; i >= 0; i--)
            for (int j = cols - 1; j >= 0; j--) {
                int best = dp[j] < dp[j + 1] ? dp[j] : dp[j + 1];
                int need = best - dungeon[i][j];
                dp[j] = need <= 1 ? 1 : need;
            }
        printf("%d", dp[0]);
        if (tc + 1 < t) printf("\n");
    }
    return 0;
}
