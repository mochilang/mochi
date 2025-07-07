// exists_builtin.c - manual translation of tests/vm/valid/exists_builtin.mochi
#include <stdio.h>
#include <stdbool.h>

int main() {
    int data[] = {1,2};
    int n = sizeof(data)/sizeof(data[0]);
    bool flag = false;
    for(int i=0;i<n;i++) {
        if(data[i]==1) { flag=true; break; }
    }
    printf(flag?"true\n":"false\n");
    return 0;
}
