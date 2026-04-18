#include <stdio.h>
#include <string.h>
#include <stdbool.h>

bool isMatch(const char* s, const char* p) {
    int i = 0, j = 0, star = -1, match = 0;
    int n = (int)strlen(s), m = (int)strlen(p);
    while (i < n) {
        if (j < m && (p[j] == '?' || p[j] == s[i])) { i++; j++; }
        else if (j < m && p[j] == '*') { star = j; match = i; j++; }
        else if (star != -1) { j = star + 1; match++; i = match; }
        else return false;
    }
    while (j < m && p[j] == '*') j++;
    return j == m;
}
int main(){int t;if(scanf("%d",&t)!=1)return 0; getchar(); for(int tc=0;tc<t;tc++){int n,m; char s[4096]={0}, p[4096]={0}; scanf("%d\n",&n); if(n>0) fgets(s,sizeof(s),stdin); if(n>0){size_t z=strcspn(s,"\r\n"); s[z]=0;} scanf("%d\n",&m); if(m>0) fgets(p,sizeof(p),stdin); if(m>0){size_t z=strcspn(p,"\r\n"); p[z]=0;} if(tc) printf("\n"); printf(isMatch(s,p)?"true":"false"); } return 0; }
