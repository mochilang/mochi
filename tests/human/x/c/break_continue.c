// break_continue.c - manual translation of tests/vm/valid/break_continue.mochi
#include <stdio.h>

int main() {
    int numbers[] = {1,2,3,4,5,6,7,8,9};
    int len = sizeof(numbers)/sizeof(numbers[0]);
    for (int i=0;i<len;i++) {
        int n = numbers[i];
        if (n % 2 == 0) continue;
        if (n > 7) break;
        printf("odd number: %d\n", n);
    }
    return 0;
}
