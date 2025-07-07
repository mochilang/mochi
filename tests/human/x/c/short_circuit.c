// short_circuit.c - manual translation of tests/vm/valid/short_circuit.mochi
#include <stdio.h>
#include <stdbool.h>

static bool boom(int a, int b) {
    printf("boom\n");
    return true;
}

int main() {
    printf("%s\n", (false && boom(1,2)) ? "true" : "false");
    printf("%s\n", (true || boom(1,2)) ? "true" : "false");
    return 0;
}
