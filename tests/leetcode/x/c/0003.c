#include <stdio.h>
#include <string.h>

static int longest(const char *s) {
    int last[256];
    for (int i = 0; i < 256; i++) last[i] = -1;
    int left = 0, best = 0;
    for (int right = 0; s[right] != '\0'; right++) {
        unsigned char ch = (unsigned char)s[right];
        if (last[ch] >= left) left = last[ch] + 1;
        last[ch] = right;
        if (right - left + 1 > best) best = right - left + 1;
    }
    return best;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    getchar();
    char s[10005];
    for (int i = 0; i < t; i++) {
        if (!fgets(s, sizeof(s), stdin)) s[0] = '\0';
        s[strcspn(s, "\r\n")] = '\0';
        printf("%d", longest(s));
        if (i + 1 < t) putchar('\n');
    }
    return 0;
}
