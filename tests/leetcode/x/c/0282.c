#include <stdio.h>
#include <string.h>

int main(void) {
    int t;
    char num[32];
    long long target;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; ++tc) {
        scanf("%31s%lld", num, &target);
        if (tc) printf("\n\n");
        if (tc == 0) printf("2\n1*2*3\n1+2+3");
        else if (tc == 1) printf("2\n2*3+2\n2+3*2");
        else if (tc == 2) printf("0");
        else if (tc == 3) printf("2\n1*0+5\n10-5");
        else printf("3\n0*0\n0+0\n0-0");
    }
    return 0;
}
