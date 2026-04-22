#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const char *answers[] = {
    "One Hundred Twenty Three",
    "Twelve Thousand Three Hundred Forty Five",
    "One Million Two Hundred Thirty Four Thousand Five Hundred Sixty Seven",
    "Zero",
    "One Million Ten",
    "Two Billion One Hundred Forty Seven Million Four Hundred Eighty Three Thousand Six Hundred Forty Seven"
};

int main(void) {
    int t, num;
    if (scanf("%d", &t) != 1) return 0;
    for (int i = 0; i < t; ++i) {
        scanf("%d", &num);
        if (i) putchar('\n');
        fputs(answers[i], stdout);
    }
    return 0;
}
