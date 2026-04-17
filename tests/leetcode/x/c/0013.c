#include <stdio.h>

static int value(char c) {
    switch (c) {
        case 'I': return 1;
        case 'V': return 5;
        case 'X': return 10;
        case 'L': return 50;
        case 'C': return 100;
        case 'D': return 500;
        case 'M': return 1000;
    }
    return 0;
}

static int roman_to_int(const char *s) {
    int total = 0;
    for (int i = 0; s[i] != '\0'; i++) {
        int cur = value(s[i]);
        int next = s[i + 1] == '\0' ? 0 : value(s[i + 1]);
        total += cur < next ? -cur : cur;
    }
    return total;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    char s[64];
    for (int i = 0; i < t; i++) {
        scanf("%63s", s);
        printf("%d\n", roman_to_int(s));
    }
    return 0;
}
