#include <stdio.h>
#include <ctype.h>
#include <limits.h>
#include <string.h>

static int my_atoi(const char *s) {
    int i = 0;
    while (s[i] == ' ') i++;
    int sign = 1;
    if (s[i] == '+' || s[i] == '-') {
        if (s[i] == '-') sign = -1;
        i++;
    }
    int ans = 0;
    int limit = sign > 0 ? 7 : 8;
    while (isdigit((unsigned char)s[i])) {
        int digit = s[i] - '0';
        if (ans > 214748364 || (ans == 214748364 && digit > limit)) return sign > 0 ? INT_MAX : INT_MIN;
        ans = ans * 10 + digit;
        i++;
    }
    return sign * ans;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    getchar();
    char s[10005];
    for (int i = 0; i < t; i++) {
        if (!fgets(s, sizeof(s), stdin)) s[0] = '\0';
        s[strcspn(s, "\r\n")] = '\0';
        printf("%d", my_atoi(s));
        if (i + 1 < t) putchar('\n');
    }
    return 0;
}
