#include <stdio.h>
#include <string.h>

static void expand(const char *s, int n, int left, int right, int *best_start, int *best_len) {
    while (left >= 0 && right < n && s[left] == s[right]) {
        left--;
        right++;
    }
    int len = right - left - 1;
    if (len > *best_len) {
        *best_len = len;
        *best_start = left + 1;
    }
}

static void longest_palindrome(const char *s, char *out) {
    int n = (int)strlen(s);
    int best_start = 0;
    int best_len = n > 0 ? 1 : 0;
    for (int i = 0; i < n; i++) {
        expand(s, n, i, i, &best_start, &best_len);
        expand(s, n, i, i + 1, &best_start, &best_len);
    }
    memcpy(out, s + best_start, (size_t)best_len);
    out[best_len] = '\0';
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    getchar();
    char s[10005];
    char out[10005];
    for (int i = 0; i < t; i++) {
        if (!fgets(s, sizeof(s), stdin)) s[0] = '\0';
        s[strcspn(s, "\r\n")] = '\0';
        longest_palindrome(s, out);
        printf("%s", out);
        if (i + 1 < t) putchar('\n');
    }
    return 0;
}
