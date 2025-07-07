#include <stdio.h>
#include <string.h>

struct Item { const char *cat; int val; int flag; };
struct Stat { const char *cat; int num; int den; };

int main() {
    struct Item items[] = {
        {"a", 10, 1},
        {"a", 5, 0},
        {"b", 20, 1}
    };
    int n = sizeof(items)/sizeof(items[0]);

    struct Stat stats[10];
    int count = 0;

    for (int i = 0; i < n; i++) {
        int j;
        for (j = 0; j < count; j++) {
            if (strcmp(stats[j].cat, items[i].cat) == 0) break;
        }
        if (j == count) {
            stats[count].cat = items[i].cat;
            stats[count].num = 0;
            stats[count].den = 0;
            count++;
        }
        if (items[i].flag)
            stats[j].num += items[i].val;
        stats[j].den += items[i].val;
    }

    // simple alphabetical sort by cat
    for (int i = 0; i < count-1; i++) {
        for (int j = i+1; j < count; j++) {
            if (strcmp(stats[i].cat, stats[j].cat) > 0) {
                struct Stat tmp = stats[i];
                stats[i] = stats[j];
                stats[j] = tmp;
            }
        }
    }

    printf("[");
    for (int i = 0; i < count; i++) {
        double share = stats[i].den ? (double)stats[i].num / stats[i].den : 0.0;
        printf("{cat: '%s', share: %.6g}%s", stats[i].cat, share,
               i == count-1 ? "" : ", ");
    }
    printf("]\n");
    return 0;
}
