#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void convert(const char *s, int numRows, char *out) {
    int n = (int)strlen(s);
    if (numRows <= 1 || numRows >= n) {
        strcpy(out, s);
        return;
    }
    int cycle = 2 * numRows - 2;
    int pos = 0;
    for (int row = 0; row < numRows; row++) {
        for (int i = row; i < n; i += cycle) {
            out[pos++] = s[i];
            int diag = i + cycle - 2 * row;
            if (row > 0 && row < numRows - 1 && diag < n) out[pos++] = s[diag];
        }
    }
    out[pos] = '\0';
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    getchar();
    char s[10005];
    char out[10005];
    char rows[32];
    for (int caseNo = 0; caseNo < t; caseNo++) {
        if (!fgets(s, sizeof(s), stdin)) s[0] = '\0';
        s[strcspn(s, "\r\n")] = '\0';
        if (!fgets(rows, sizeof(rows), stdin)) strcpy(rows, "1");
        int numRows = atoi(rows);
        convert(s, numRows, out);
        printf("%s", out);
        if (caseNo + 1 < t) putchar('\n');
    }
    return 0;
}
