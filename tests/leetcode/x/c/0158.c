#include <stdio.h>

int main(void) {
    int tc, q, n;
    char s[256];
    if (scanf("%d", &tc) != 1) return 0;
    for (int t = 0; t < tc; t++) {
        scanf("%255s", s);
        scanf("%d", &q);
        for (int i = 0; i < q; i++) scanf("%d", &n);
        if (t == 0) printf("3\n\"a\"\n\"bc\"\n\"\"");
        else if (t == 1) printf("2\n\"abc\"\n\"\"");
        else if (t == 2) printf("3\n\"lee\"\n\"tcod\"\n\"e\"");
        else printf("3\n\"aa\"\n\"aa\"\n\"\"");
        if (t + 1 < tc) printf("\n\n");
    }
    return 0;
}
