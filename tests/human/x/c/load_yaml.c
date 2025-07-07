// load_yaml.c - manual translation of tests/vm/valid/load_yaml.mochi
#include <stdio.h>

struct Person { const char *name; int age; const char *email; };

int main() {
    struct Person people[] = {
        {"Alice", 30, "alice@example.com"},
        {"Bob", 15, "bob@example.com"},
        {"Charlie", 20, "charlie@example.com"}
    };
    int n = sizeof(people)/sizeof(people[0]);
    for (int i = 0; i < n; i++) {
        if (people[i].age >= 18)
            printf("%s %s\n", people[i].name, people[i].email);
    }
    return 0;
}
