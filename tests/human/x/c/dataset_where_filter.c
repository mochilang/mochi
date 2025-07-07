// dataset_where_filter.c - manual translation of tests/vm/valid/dataset_where_filter.mochi
#include <stdio.h>
#include <stdbool.h>

struct Person { const char *name; int age; };

int main() {
    struct Person people[] = {
        {"Alice",30},
        {"Bob",15},
        {"Charlie",65},
        {"Diana",45}
    };
    int n = sizeof(people)/sizeof(people[0]);
    printf("--- Adults ---\n");
    for(int i=0;i<n;i++) {
        if(people[i].age >= 18) {
            bool is_senior = people[i].age >= 60;
            printf("%s is %d%s\n", people[i].name, people[i].age, is_senior?" (senior)":"");
        }
    }
    return 0;
}
