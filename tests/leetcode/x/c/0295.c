#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int *data;
    int size;
    int cap;
} MedianFinder;

static void add_num(MedianFinder *mf, int num) {
    if (mf->size == mf->cap) {
        mf->cap = mf->cap ? mf->cap * 2 : 8;
        mf->data = realloc(mf->data, (size_t)mf->cap * sizeof(int));
    }
    int lo = 0, hi = mf->size;
    while (lo < hi) {
        int mid = (lo + hi) / 2;
        if (mf->data[mid] < num) lo = mid + 1;
        else hi = mid;
    }
    memmove(&mf->data[lo + 1], &mf->data[lo], (size_t)(mf->size - lo) * sizeof(int));
    mf->data[lo] = num;
    mf->size++;
}

static double find_median(const MedianFinder *mf) {
    if (mf->size % 2 == 1) return mf->data[mf->size / 2];
    return (mf->data[mf->size / 2 - 1] + mf->data[mf->size / 2]) / 2.0;
}

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int m;
        scanf("%d", &m);
        MedianFinder mf = {0};
        if (tc) printf("\n\n");
        int first = 1;
        for (int i = 0; i < m; ++i) {
            char op[32];
            scanf("%31s", op);
            if (strcmp(op, "addNum") == 0) {
                int x;
                scanf("%d", &x);
                add_num(&mf, x);
            } else {
                if (!first) printf("\n");
                first = 0;
                printf("%.1f", find_median(&mf));
            }
        }
        free(mf.data);
    }
    return 0;
}
