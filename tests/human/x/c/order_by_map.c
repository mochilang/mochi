#include <stdio.h>
#include <stdlib.h>

struct Pair { int a; int b; };

int cmp_pair(const void *x, const void *y) {
    const struct Pair *px = x; const struct Pair *py = y;
    if (px->a != py->a) return px->a - py->a;
    return px->b - py->b;
}

int main() {
    struct Pair data[] = {
        {1,2},
        {1,1},
        {0,5}
    };
    int n = sizeof(data)/sizeof(data[0]);
    qsort(data,n,sizeof(struct Pair),cmp_pair);
    printf("[");
    for(int i=0;i<n;i++) {
        printf("{a:%d,b:%d}%s", data[i].a, data[i].b, i==n-1?"":", ");
    }
    printf("]\n");
    return 0;
}
