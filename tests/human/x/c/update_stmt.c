// update_stmt.c - manual translation of tests/vm/valid/update_stmt.mochi
#include <stdio.h>
#include <string.h>
#include <assert.h>

struct Person { const char *name; int age; char status[10]; };

int main() {
    struct Person people[] = {
        {"Alice", 17, "minor"},
        {"Bob", 25, "unknown"},
        {"Charlie", 18, "unknown"},
        {"Diana", 16, "minor"}
    };
    int n = sizeof(people)/sizeof(people[0]);

    for (int i = 0; i < n; i++) {
        if (people[i].age >= 18) {
            strcpy(people[i].status, "adult");
            people[i].age += 1;
        }
    }

    struct Person expected[] = {
        {"Alice", 17, "minor"},
        {"Bob", 26, "adult"},
        {"Charlie", 19, "adult"},
        {"Diana", 16, "minor"}
    };
    for (int i = 0; i < n; i++) {
        assert(people[i].age == expected[i].age &&
               strcmp(people[i].status, expected[i].status) == 0 &&
               strcmp(people[i].name, expected[i].name) == 0);
    }
    printf("ok\n");
    return 0;
}
