// tail_recursion.c - manual translation of tests/vm/valid/tail_recursion.mochi
#include <stdio.h>

int sum_rec(int n, int acc) {
    if(n==0) return acc;
    return sum_rec(n-1, acc + n);
}

int main() {
    printf("%d\n", sum_rec(10,0));
    return 0;
}
