#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct Person { const char *name; const char *city; };
struct Stat { const char *city; int count; };

int main() {
    struct Person people[] = {
        {"Alice", "Paris"},
        {"Bob", "Hanoi"},
        {"Charlie", "Paris"},
        {"Diana", "Hanoi"},
        {"Eve", "Paris"},
        {"Frank", "Hanoi"},
        {"George", "Paris"}
    };
    int n = sizeof(people)/sizeof(people[0]);

    struct Stat stats[10];
    int count = 0;
    for (int i = 0; i < n; i++) {
        int j;
        for (j = 0; j < count; j++) if (strcmp(stats[j].city, people[i].city) == 0) break;
        if (j == count) { stats[count].city = people[i].city; stats[count].count = 0; count++; }
        stats[j].count++;
    }

    printf("[");
    int first = 1;
    for (int i = 0; i < count; i++) {
        if (stats[i].count >= 4) {
            if (!first) printf(", ");
            first = 0;
            printf("{city: '%s', num: %d}", stats[i].city, stats[i].count);
        }
    }
    printf("]\n");
    return 0;
}
