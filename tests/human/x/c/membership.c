// membership.c - manual translation of tests/vm/valid/membership.mochi
#include <stdio.h>
#include <stdbool.h>

static bool contains(int *arr, int n, int value) {
    for (int i = 0; i < n; i++) {
        if (arr[i] == value) return true;
    }
    return false;
}

int main() {
    int nums[] = {1, 2, 3};
    printf("%s\n", contains(nums, 3, 2) ? "true" : "false");
    printf("%s\n", contains(nums, 3, 4) ? "true" : "false");
    return 0;
}
