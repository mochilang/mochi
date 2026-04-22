#include <stdio.h>
#include <string.h>

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int r, c;
        scanf("%d %d", &r, &c);
        char image[128][128];
        for (int i = 0; i < r; ++i) scanf("%127s", image[i]);
        int x, y;
        scanf("%d %d", &x, &y);
        int top = r, bottom = -1, left = c, right = -1;
        for (int i = 0; i < r; ++i) {
            for (int j = 0; j < (int)strlen(image[i]); ++j) {
                if (image[i][j] == '1') {
                    if (i < top) top = i;
                    if (i > bottom) bottom = i;
                    if (j < left) left = j;
                    if (j > right) right = j;
                }
            }
        }
        if (tc) printf("\n\n");
        printf("%d", (bottom - top + 1) * (right - left + 1));
    }
    return 0;
}
