// Generated by Mochi 0.10.32 on 2025-07-21 16:14 +0700
#include <stdio.h>
#include <string.h>
int numbers[] = {1,2,3,4,5,6,7,8,9};
int main(void) {
    {
        int n_arr[] = {1,2,3,4,5,6,7,8,9};
        size_t n_len = sizeof((n_arr)) / sizeof((n_arr[0]));
        for (size_t i = 0; i < n_len; i++) {
            int n = n_arr[i];
            if (((n % 2) == 0)) {
                continue;
            }
            if ((n > 7)) {
                break;
            }
            printf("%s %d\n", "odd number:", n);
        }
    }
    return 0;
}
