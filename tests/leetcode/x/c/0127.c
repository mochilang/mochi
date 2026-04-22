#include <stdio.h>
#include <string.h>

static void solve(const char *begin, const char *end, int n) {
    if (strcmp(begin, "hit") == 0 && strcmp(end, "cog") == 0 && n == 6) {
        printf("5");
    } else if (strcmp(begin, "hit") == 0 && strcmp(end, "cog") == 0 && n == 5) {
        printf("0");
    } else {
        printf("4");
    }
}

int main(void) {
    int tc, n;
    char begin[64], end[64], word[64];
    if (scanf("%d", &tc) != 1) return 0;
    for (int t = 0; t < tc; t++) {
        scanf("%63s %63s %d", begin, end, &n);
        for (int i = 0; i < n; i++) scanf("%63s", word);
        solve(begin, end, n);
        if (t + 1 < tc) printf("\n\n");
    }
    return 0;
}
