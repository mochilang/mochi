#include <stdio.h>
#include <string.h>
int solveCase(const char *s){ int stack[40000], top=0, best=0; stack[top++] = -1; for(int i=0;s[i];++i){ if(s[i]=='(') stack[top++]=i; else { --top; if(top==0) stack[top++]=i; else if(i-stack[top-1] > best) best=i-stack[top-1]; } } return best; }
int main(void){ int t; if(scanf("%d", &t)!=1) return 0; for(int tc=0; tc<t; ++tc){ int n; char s[40005]=""; scanf("%d", &n); if(n>0) scanf("%40000s", s); printf("%d", solveCase(s)); if(tc+1<t) printf("\n"); } return 0; }
