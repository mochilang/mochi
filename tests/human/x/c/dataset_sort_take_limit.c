// dataset_sort_take_limit.c - manual translation of tests/vm/valid/dataset_sort_take_limit.mochi
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct Product { const char *name; int price; };

int cmp_desc(const void *a, const void *b) {
    const struct Product *pa = a; const struct Product *pb = b;
    return pb->price - pa->price;
}

int main() {
    struct Product products[] = {
        {"Laptop",1500},
        {"Smartphone",900},
        {"Tablet",600},
        {"Monitor",300},
        {"Keyboard",100},
        {"Mouse",50},
        {"Headphones",200}
    };
    int n = sizeof(products)/sizeof(products[0]);
    qsort(products,n,sizeof(struct Product),cmp_desc);
    printf("--- Top products (excluding most expensive) ---\n");
    for(int i=1;i<1+3 && i<n;i++) {
        printf("%s costs $%d\n", products[i].name, products[i].price);
    }
    return 0;
}
