#include <stdio.h>
#include <string.h>

static int is_valid(const char *s) {
    char stack[10005];
    int top = 0;
    for (int i = 0; s[i] != '\0'; i++) {
        char ch = s[i];
        if (ch == '(' || ch == '[' || ch == '{') {
            stack[top++] = ch;
        } else {
            if (top == 0) return 0;
            char open = stack[--top];
            if ((ch == ')' && open != '(') ||
                (ch == ']' && open != '[') ||
                (ch == '}' && open != '{')) return 0;
        }
    }
    return top == 0;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    char s[10005];
    for (int i = 0; i < t; i++) {
        scanf("%10004s", s);
        printf("%s\n", is_valid(s) ? "true" : "false");
    }
    return 0;
}
