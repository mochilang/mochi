#include <iostream>
#include <vector>
#include <string>
using namespace std;
void dfs(int r,int n,vector<int>& cols,vector<int>& d1,vector<int>& d2,vector<string>& board,vector<vector<string>>& res){ if(r==n){ res.push_back(board); return; } for(int c=0;c<n;c++){ int a=r+c,b=r-c+n-1; if(cols[c]||d1[a]||d2[b]) continue; cols[c]=d1[a]=d2[b]=1; board[r][c]='Q'; dfs(r+1,n,cols,d1,d2,board,res); board[r][c]='.'; cols[c]=d1[a]=d2[b]=0; } }
vector<vector<string>> solve(int n){ vector<int> cols(n),d1(2*n),d2(2*n); vector<string> board(n,string(n,'.')); vector<vector<string>> res; dfs(0,n,cols,d1,d2,board,res); return res; }
int main(){ ios::sync_with_stdio(false); cin.tie(nullptr); int t; if(!(cin>>t)) return 0; for(int tc=0; tc<t; tc++){ int n; cin>>n; auto sols=solve(n); cout<<sols.size(); if(tc + 1 < t) cout<<'\n'; } return 0; }
