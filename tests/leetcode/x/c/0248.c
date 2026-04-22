#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const char PAIRS[5][2] = {{'0', '0'}, {'1', '1'}, {'6', '9'}, {'8', '8'}, {'9', '6'}};

static int count_build(char *buf, int left, int right, int len, const char *low, const char *high) {
    if (left > right) {
        buf[len] = '\0';
        if ((len == (int)strlen(low) && strcmp(buf, low) < 0) || (len == (int)strlen(high) && strcmp(buf, high) > 0)) return 0;
        return 1;
    }
    int ans = 0;
    for (int i = 0; i < 5; ++i) {
        char a = PAIRS[i][0], b = PAIRS[i][1];
        if (left == 0 && len > 1 && a == '0') continue;
        if (left == right && a != b) continue;
        if (left == right && (a == '6' || a == '9')) continue;
        buf[left] = a;
        buf[right] = b;
        ans += count_build(buf, left + 1, right - 1, len, low, high);
    }
    return ans;
}

static int count_range(const char *low, const char *high) {
    int ans = 0;
    char buf[32];
    for (int len = (int)strlen(low); len <= (int)strlen(high); ++len) {
        ans += count_build(buf, 0, len - 1, len, low, high);
    }
    return ans;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    char low[32], high[32];
    for (int i = 0; i < t; ++i) {
        scanf("%31s %31s", low, high);
        if (i) putchar('\n');
        printf("%d", count_range(low, high));
    }
    return 0;
}
