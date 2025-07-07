// slice.c - manual translation of tests/vm/valid/slice.mochi
#include <stdio.h>
#include <string.h>

static void print_list(int *arr, int start, int end) {
    printf("[");
    for (int i = start; i < end; i++) {
        if (i > start) printf(", ");
        printf("%d", arr[i]);
    }
    printf("]\n");
}

int main() {
    int nums[] = {1, 2, 3};
    print_list(nums, 1, 3);
    print_list(nums, 0, 2);

    const char *s = "hello";
    char buf[5];
    strncpy(buf, s + 1, 3);
    buf[3] = '\0';
    printf("%s\n", buf);
    return 0;
}
