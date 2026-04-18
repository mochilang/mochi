#include <stdio.h>
#include <string.h>
#include <stdbool.h>

bool isNumber(char *s) {
    bool seenDigit = false, seenDot = false, seenExp = false, digitAfterExp = true;
    int n = strlen(s);
    for (int i = 0; i < n; i++) {
        char ch = s[i];
        if (ch >= '0' && ch <= '9') {
            seenDigit = true;
            if (seenExp) digitAfterExp = true;
        } else if (ch == '+' || ch == '-') {
            if (i != 0 && s[i - 1] != 'e' && s[i - 1] != 'E') return false;
        } else if (ch == '.') {
            if (seenDot || seenExp) return false;
            seenDot = true;
        } else if (ch == 'e' || ch == 'E') {
            if (seenExp || !seenDigit) return false;
            seenExp = true;
            digitAfterExp = false;
        } else return false;
    }
    return seenDigit && digitAfterExp;
}

int main() {
    int t;
    if (scanf("%d\n", &t) != 1) return 0;
    char s[128];
    for (int i = 0; i < t; i++) {
        if (!fgets(s, sizeof(s), stdin)) break;
        s[strcspn(s, "\r\n")] = 0;
        printf(isNumber(s) ? "true" : "false");
        if (i + 1 < t) printf("\n");
    }
    return 0;
}
