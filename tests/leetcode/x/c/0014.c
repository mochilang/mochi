#include <stdio.h>
#include <string.h>

static int starts_with(const char *s, const char *p) {
    while (*p) {
        if (*s++ != *p++) return 0;
    }
    return 1;
}

int main(void) {
    int t, n;
    if (scanf("%d", &t) != 1) return 0;
    char strs[256][256];
    char prefix[256];
    for (int tc = 0; tc < t; tc++) {
        scanf("%d", &n);
        for (int i = 0; i < n; i++) scanf("%255s", strs[i]);
        strcpy(prefix, strs[0]);
        while (1) {
            int ok = 1;
            for (int i = 1; i < n; i++) {
                if (!starts_with(strs[i], prefix)) {
                    ok = 0;
                    break;
                }
            }
            if (ok) break;
            prefix[strlen(prefix) - 1] = '\0';
        }
        printf("\"%s\"\n", prefix);
    }
    return 0;
}
