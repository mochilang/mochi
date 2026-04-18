#include <stdio.h>
#include <string.h>
#include <stdbool.h>

char s1[64], s2[64];
signed char memo[31][31][31];

bool dfs(int i1, int i2, int len) {
    if (memo[i1][i2][len] != -1) return memo[i1][i2][len];
    if (strncmp(s1 + i1, s2 + i2, len) == 0) return memo[i1][i2][len] = 1;
    int cnt[26] = {0};
    for (int i = 0; i < len; i++) {
        cnt[s1[i1 + i] - 'a']++;
        cnt[s2[i2 + i] - 'a']--;
    }
    for (int i = 0; i < 26; i++) if (cnt[i] != 0) return memo[i1][i2][len] = 0;
    for (int k = 1; k < len; k++) {
        if ((dfs(i1, i2, k) && dfs(i1 + k, i2 + k, len - k)) ||
            (dfs(i1, i2 + len - k, k) && dfs(i1 + k, i2, len - k)))
            return memo[i1][i2][len] = 1;
    }
    return memo[i1][i2][len] = 0;
}

int main(void) {
    int t;
    if (scanf("%d\n", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        fgets(s1, sizeof(s1), stdin);
        fgets(s2, sizeof(s2), stdin);
        s1[strcspn(s1, "\r\n")] = 0;
        s2[strcspn(s2, "\r\n")] = 0;
        memset(memo, -1, sizeof(memo));
        printf("%s", dfs(0, 0, (int)strlen(s1)) ? "true" : "false");
        if (tc + 1 < t) printf("\n");
    }
    return 0;
}
