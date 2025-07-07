// in_operator_extended.c - manual translation of tests/vm/valid/in_operator_extended.mochi
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

static bool contains_int(const int *arr, int size, int val) {
    for (int i = 0; i < size; i++) if (arr[i] == val) return true;
    return false;
}

static bool map_contains(const char *key) {
    const char *keys[] = {"a"};
    for (int i = 0; i < 1; i++) if (strcmp(keys[i], key) == 0) return true;
    return false;
}

int main() {
    int xs[] = {1, 2, 3};
    int ys[] = {1, 3};
    printf("%s\n", contains_int(ys, 2, 1) ? "true" : "false");
    printf("%s\n", contains_int(ys, 2, 2) ? "true" : "false");

    printf("%s\n", map_contains("a") ? "true" : "false");
    printf("%s\n", map_contains("b") ? "true" : "false");

    const char *s = "hello";
    printf("%s\n", strstr(s, "ell") ? "true" : "false");
    printf("%s\n", strstr(s, "foo") ? "true" : "false");
    return 0;
}
