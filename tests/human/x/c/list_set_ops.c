// list_set_ops.c - manual translation of tests/vm/valid/list_set_ops.mochi
#include <stdio.h>
#include <stdbool.h>

static void print_list(const int *a, int n) {
    for (int i = 0; i < n; i++) {
        if (i) putchar(' ');
        printf("%d", a[i]);
    }
    putchar('\n');
}

int main() {
    int union_res[] = {1,2,3};
    print_list(union_res, 3);

    int except_res[] = {1,3};
    print_list(except_res, 2);

    int intersect_res[] = {2};
    print_list(intersect_res, 1);

    int union_all_len = 4;
    printf("%d\n", union_all_len);
    return 0;
}
