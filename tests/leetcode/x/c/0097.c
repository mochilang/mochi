#include <stdio.h>
#include <string.h>
#include <stdbool.h>

static bool solve(const char *s1, const char *s2, const char *s3) {
    int m = (int)strlen(s1), n = (int)strlen(s2);
    if (m + n != (int)strlen(s3)) return false;
    bool dp[128][128] = {{false}};
    dp[0][0] = true;
    for (int i = 0; i <= m; i++) {
        for (int j = 0; j <= n; j++) {
            if (i > 0 && dp[i - 1][j] && s1[i - 1] == s3[i + j - 1]) dp[i][j] = true;
            if (j > 0 && dp[i][j - 1] && s2[j - 1] == s3[i + j - 1]) dp[i][j] = true;
        }
    }
    return dp[m][n];
}

int main(void) {
    int t;
    char s1[256], s2[256], s3[256];
    if (scanf("%d\n", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        fgets(s1, sizeof(s1), stdin);
        fgets(s2, sizeof(s2), stdin);
        fgets(s3, sizeof(s3), stdin);
        s1[strcspn(s1, "\r\n")] = 0;
        s2[strcspn(s2, "\r\n")] = 0;
        s3[strcspn(s3, "\r\n")] = 0;
        printf("%s", solve(s1, s2, s3) ? "true" : "false");
        if (tc + 1 < t) printf("\n");
    }
    return 0;
}
