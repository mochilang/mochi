#include <stdio.h>
#include <string.h>
#include <stdbool.h>

static bool matchAt(const char *s, const char *p, int i, int j) {
    if (p[j] == '\0') return s[i] == '\0';
    bool first = s[i] != '\0' && (p[j] == '.' || s[i] == p[j]);
    if (p[j + 1] == '*') {
        return matchAt(s, p, i, j + 2) || (first && matchAt(s, p, i + 1, j));
    }
    return first && matchAt(s, p, i + 1, j + 1);
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    char s[256], p[256];
    for (int tc = 0; tc < t; ++tc) {
        scanf("%255s", s);
        scanf("%255s", p);
        printf(matchAt(s, p, 0, 0) ? "true" : "false");
        if (tc + 1 < t) printf("\n");
    }
    return 0;
}
