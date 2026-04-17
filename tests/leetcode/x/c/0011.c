#include <stdio.h>

static int max_area(int *h, int n) {
    int left = 0, right = n - 1, best = 0;
    while (left < right) {
        int height = h[left] < h[right] ? h[left] : h[right];
        int area = (right - left) * height;
        if (area > best) best = area;
        if (h[left] < h[right]) left++; else right--;
    }
    return best;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n;
        scanf("%d", &n);
        int h[100005];
        for (int i = 0; i < n; i++) scanf("%d", &h[i]);
        printf("%d", max_area(h, n));
        if (tc + 1 < t) putchar('\n');
    }
    return 0;
}
