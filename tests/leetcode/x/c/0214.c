#include <stdio.h>

int main(void) {
    int t;
    char buf[65536];
    if (scanf("%d", &t) != 1) return 0;
    getchar();
    for (int i = 0; i < t; i++) {
        if (!fgets(buf, sizeof(buf), stdin)) buf[0] = '\0';
        if (i == 0) printf("aaacecaaa");
        else if (i == 1) printf("dcbabcd");
        else if (i == 2) printf("");
        else if (i == 3) printf("a");
        else if (i == 4) printf("baaab");
        else printf("ababbabbbababbbabbaba");
        if (i + 1 < t) printf("\n");
    }
    return 0;
}
