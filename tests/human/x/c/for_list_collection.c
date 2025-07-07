// for_list_collection.c - manual translation of tests/vm/valid/for_list_collection.mochi
#include <stdio.h>

int main() {
    int arr[] = {1, 2, 3};
    for (int i = 0; i < 3; i++) {
        printf("%d\n", arr[i]);
    }
    return 0;
}
