#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int calculate(const char *expr) {
    int result = 0, number = 0, sign = 1;
    int stack[100000];
    int top = 0;
    for (int i = 0; expr[i] != '\0'; ++i) {
        char ch = expr[i];
        if (isdigit((unsigned char)ch)) {
            number = number * 10 + (ch - '0');
        } else if (ch == '+' || ch == '-') {
            result += sign * number;
            number = 0;
            sign = ch == '+' ? 1 : -1;
        } else if (ch == '(') {
            stack[top++] = result;
            stack[top++] = sign;
            result = 0;
            number = 0;
            sign = 1;
        } else if (ch == ')') {
            result += sign * number;
            number = 0;
            int prevSign = stack[--top];
            int prevResult = stack[--top];
            result = prevResult + prevSign * result;
        }
    }
    return result + sign * number;
}

int main(void) {
    char line[1 << 20];
    if (!fgets(line, sizeof(line), stdin)) return 0;
    int t = atoi(line);
    for (int i = 0; i < t; ++i) {
        if (!fgets(line, sizeof(line), stdin)) break;
        line[strcspn(line, "\r\n")] = '\0';
        if (i) putchar('\n');
        printf("%d", calculate(line));
    }
    return 0;
}
