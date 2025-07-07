// in_operator.c - manual translation of tests/vm/valid/in_operator.mochi
#include <stdio.h>
#include <stdbool.h>

static bool contains(int arr[], int size, int val) {
    for (int i = 0; i < size; i++) {
        if (arr[i] == val) return true;
    }
    return false;
}

int main() {
    int xs[] = {1, 2, 3};
    printf("%s\n", contains(xs, 3, 2) ? "true" : "false");
    printf("%s\n", (!contains(xs, 3, 5)) ? "true" : "false");
    return 0;
}
