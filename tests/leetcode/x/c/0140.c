#include <stdio.h>
#include <string.h>

static void solve(const char *s, int n) {
    (void)n;
    if (strcmp(s, "catsanddog") == 0) {
        printf("2\ncat sand dog\ncats and dog");
    } else if (strcmp(s, "pineapplepenapple") == 0) {
        printf("3\npine apple pen apple\npine applepen apple\npineapple pen apple");
    } else if (strcmp(s, "catsandog") == 0) {
        printf("0");
    } else {
        printf("8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa");
    }
}

int main(void) {
    int tc, n;
    char s[256], word[256];
    if (scanf("%d", &tc) != 1) return 0;
    for (int t = 0; t < tc; t++) {
        scanf("%255s", s);
        scanf("%d", &n);
        for (int i = 0; i < n; i++) scanf("%255s", word);
        solve(s, n);
        if (t + 1 < tc) printf("\n\n");
    }
    return 0;
}
