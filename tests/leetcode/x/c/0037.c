#include <stdio.h>
int valid(char b[9][10], int r,int c,char ch){ for(int i=0;i<9;i++) if(b[r][i]==ch||b[i][c]==ch) return 0; int br=(r/3)*3, bc=(c/3)*3; for(int i=br;i<br+3;i++) for(int j=bc;j<bc+3;j++) if(b[i][j]==ch) return 0; return 1; }
int solve(char b[9][10]){ for(int r=0;r<9;r++) for(int c=0;c<9;c++) if(b[r][c]=='.'){ for(char ch='1'; ch<='9'; ch++) if(valid(b,r,c,ch)){ b[r][c]=ch; if(solve(b)) return 1; b[r][c]='.'; } return 0; } return 1; }
int main(void){ int t; if(scanf("%d", &t)!=1) return 0; for(int tc=0; tc<t; tc++){ char b[9][10]; for(int i=0;i<9;i++) scanf("%9s", b[i]); solve(b); for(int i=0;i<9;i++){ printf("%s", b[i]); if(tc+1<t||i<8) printf("\n"); } } return 0; }
