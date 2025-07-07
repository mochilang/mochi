// bool_chain.c - manual translation of tests/vm/valid/bool_chain.mochi
#include <stdio.h>
#include <stdbool.h>

static bool boom() {
    printf("boom\n");
    return true;
}

int main() {
    printf("%s\n", (1 < 2 && 2 < 3 && 3 < 4) ? "true" : "false");
    printf("%s\n", (1 < 2 && 2 > 3 && boom()) ? "true" : "false");
    printf("%s\n", (1 < 2 && 2 < 3 && 3 > 4 && boom()) ? "true" : "false");
    return 0;
}
