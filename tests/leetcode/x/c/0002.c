#include <stdio.h>

static int add_lists(const int *a, int n, const int *b, int m, int *out) {
    int i = 0, j = 0, k = 0, carry = 0;
    while (i < n || j < m || carry) {
        int sum = carry;
        if (i < n) sum += a[i++];
        if (j < m) sum += b[j++];
        out[k++] = sum % 10;
        carry = sum / 10;
    }
    return k;
}

static void print_arr(const int *a, int n) {
    putchar('[');
    for (int i = 0; i < n; i++) {
        if (i) putchar(',');
        printf("%d", a[i]);
    }
    puts("]");
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    while (t--) {
        int n, m, a[128], b[128], outv[256];
        scanf("%d", &n);
        for (int i = 0; i < n; i++) scanf("%d", &a[i]);
        scanf("%d", &m);
        for (int i = 0; i < m; i++) scanf("%d", &b[i]);
        int k = add_lists(a, n, b, m, outv);
        print_arr(outv, k);
    }
    return 0;
}
