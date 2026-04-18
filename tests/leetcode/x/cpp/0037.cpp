#include <iostream>
#include <vector>
using namespace std;
bool valid(vector<string>& b,int r,int c,char ch){ for(int i=0;i<9;i++) if(b[r][i]==ch||b[i][c]==ch) return false; int br=(r/3)*3, bc=(c/3)*3; for(int i=br;i<br+3;i++) for(int j=bc;j<bc+3;j++) if(b[i][j]==ch) return false; return true; }
bool solve(vector<string>& b){ for(int r=0;r<9;r++) for(int c=0;c<9;c++) if(b[r][c]=='.'){ for(char ch='1'; ch<='9'; ch++) if(valid(b,r,c,ch)){ b[r][c]=ch; if(solve(b)) return true; b[r][c]='.'; } return false; } return true; }
int main(){ int t; if(!(cin>>t)) return 0; for(int tc=0; tc<t; ++tc){ vector<string> b(9); for(int i=0;i<9;i++) cin>>b[i]; solve(b); for(int i=0;i<9;i++){ cout<<b[i]; if(tc+1<t||i<8) cout<<'\n'; } } }
