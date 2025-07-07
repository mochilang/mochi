#include <stdio.h>
#include <string.h>

struct Item { const char *cat; int val; };
struct Stat { const char *cat; int total; };

int main() {
    struct Item items[] = {
        {"a",3}, {"a",1}, {"b",5}, {"b",2}
    };
    int n = sizeof(items)/sizeof(items[0]);
    struct Stat stats[10];
    int count = 0;
    for(int i=0;i<n;i++) {
        int j;
        for(j=0;j<count;j++) if(strcmp(stats[j].cat,items[i].cat)==0) break;
        if(j==count) { stats[count].cat=items[i].cat; stats[count].total=0; count++; }
        stats[j].total += items[i].val;
    }
    // sort by total descending
    for(int i=0;i<count-1;i++)
        for(int j=i+1;j<count;j++)
            if(stats[i].total < stats[j].total){
                struct Stat t=stats[i]; stats[i]=stats[j]; stats[j]=t;
            }
    printf("[");
    for(int i=0;i<count;i++)
        printf("{cat:'%s', total:%d}%s", stats[i].cat, stats[i].total, i==count-1?"":" ,");
    printf("]\n");
    return 0;
}
