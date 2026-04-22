#include <stdio.h>
#include <string.h>

static int solve(const char *s, const char *t) {
    int n = (int)strlen(t);
    int dp[1005] = {0};
    dp[0] = 1;
    for (int i = 0; s[i]; i++) {
        for (int j = n; j >= 1; j--) {
            if (s[i] == t[j - 1]) dp[j] += dp[j - 1];
        }
    }
    return dp[n];
}

int main(void) {
    int tc;
    char s[1005], t[1005];
    if (scanf("%d\n", &tc) != 1) return 0;
    for (int i = 0; i < tc; i++) {
        fgets(s, sizeof(s), stdin);
        fgets(t, sizeof(t), stdin);
        s[strcspn(s, "\r\n")] = 0;
        t[strcspn(t, "\r\n")] = 0;
        printf("%d", solve(s, t));
        if (i + 1 < tc) putchar('\n');
    }
    return 0;
}
