// group_by.c - manual translation of tests/vm/valid/group_by.mochi
#include <stdio.h>
#include <string.h>

struct Person { const char *name; int age; const char *city; };
struct Stats { const char *city; int count; double age_sum; };

int main() {
    struct Person people[] = {
        {"Alice",30,"Paris"},
        {"Bob",15,"Hanoi"},
        {"Charlie",65,"Paris"},
        {"Diana",45,"Hanoi"},
        {"Eve",70,"Paris"},
        {"Frank",22,"Hanoi"}
    };
    int n = sizeof(people)/sizeof(people[0]);
    struct Stats stats[6];
    int num_stats = 0;
    for(int i=0;i<n;i++) {
        int found=-1;
        for(int j=0;j<num_stats;j++) {
            if(strcmp(stats[j].city, people[i].city)==0) { found=j; break; }
        }
        if(found==-1) {
            found = num_stats++;
            stats[found].city = people[i].city;
            stats[found].count=0;
            stats[found].age_sum=0.0;
        }
        stats[found].count++;
        stats[found].age_sum += people[i].age;
    }
    printf("--- People grouped by city ---\n");
    for(int i=0;i<num_stats;i++) {
        double avg_age = stats[i].age_sum / stats[i].count;
        printf("%s : count = %d , avg_age = %.17g\n", stats[i].city, stats[i].count, avg_age);
    }
    return 0;
}
