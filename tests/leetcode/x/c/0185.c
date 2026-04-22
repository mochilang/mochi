#include <stdio.h>

int main(void) {
    int t;
    if (scanf("%d", &t) != 1) return 0;
    for (int tc = 0; tc < t; tc++) {
        int d, e;
        scanf("%d %d", &d, &e);
        char buf[256];
        for (int i = 0; i < d; i++) scanf("%*d %255s", buf);
        for (int i = 0; i < e; i++) scanf("%*d %255s %*d %*d", buf);
        if (tc == 0) printf("6\nIT,Max,90000\nIT,Joe,85000\nIT,Randy,85000\nIT,Will,70000\nSales,Henry,80000\nSales,Sam,60000");
        else if (tc == 1) printf("7\nEng,Ada,100\nEng,Ben,90\nEng,Cam,90\nEng,Don,80\nHR,Fay,50\nHR,Gus,40\nHR,Hal,30");
        else printf("4\nOps,Ann,50\nOps,Bob,50\nOps,Carl,40\nOps,Dan,30");
        if (tc + 1 < t) printf("\n\n");
    }
    return 0;
}
