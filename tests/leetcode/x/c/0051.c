#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct { char ***data; int size; int cap; } Solutions;
void push_solution(Solutions *s, char **board, int n){ if(s->size==s->cap){ s->cap=s->cap? s->cap*2:4; s->data=realloc(s->data,sizeof(char**)*s->cap);} char **sol=malloc(sizeof(char*)*n); for(int i=0;i<n;i++){ sol[i]=strdup(board[i]); } s->data[s->size++]=sol; }
void dfs(int r,int n,int* cols,int* d1,int* d2,char** board,Solutions* res){ if(r==n){ push_solution(res,board,n); return; } for(int c=0;c<n;c++){ int a=r+c,b=r-c+n-1; if(cols[c]||d1[a]||d2[b]) continue; cols[c]=d1[a]=d2[b]=1; board[r][c]='Q'; dfs(r+1,n,cols,d1,d2,board,res); board[r][c]='.'; cols[c]=d1[a]=d2[b]=0; } }
int main(){ int t; if(scanf("%d",&t)!=1) return 0; for(int tc=0; tc<t; tc++){ int n; scanf("%d",&n); int *cols=calloc(n,sizeof(int)), *d1=calloc(2*n,sizeof(int)), *d2=calloc(2*n,sizeof(int)); char **board=malloc(sizeof(char*)*n); for(int i=0;i<n;i++){ board[i]=malloc(n+1); memset(board[i],'.',n); board[i][n]='\0'; } Solutions res={0}; dfs(0,n,cols,d1,d2,board,&res); if(tc) printf("=\n"); printf("%d\n",res.size); for(int si=0; si<res.size; si++){ for(int i=0;i<n;i++) printf("%s\n",res.data[si][i]); if(si+1<res.size) printf("-\n"); } for(int si=0; si<res.size; si++){ for(int i=0;i<n;i++) free(res.data[si][i]); free(res.data[si]); } free(res.data); for(int i=0;i<n;i++) free(board[i]); free(board); free(cols); free(d1); free(d2); } return 0; }
