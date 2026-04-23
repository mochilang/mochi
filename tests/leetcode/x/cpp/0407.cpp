#include <iostream>
#include <queue>
#include <tuple>
#include <vector>
using namespace std;int trap(vector<vector<int>>&g){int m=g.size(),n=g[0].size();if(m<3||n<3)return 0;vector<vector<int>>seen(m,vector<int>(n));using T=tuple<int,int,int>;priority_queue<T,vector<T>,greater<T>>pq;auto add=[&](int r,int c){if(!seen[r][c]){seen[r][c]=1;pq.push({g[r][c],r,c});}};for(int r=0;r<m;r++){add(r,0);add(r,n-1);}for(int c=0;c<n;c++){add(0,c);add(m-1,c);}int water=0;int d[5]={1,0,-1,0,1};while(!pq.empty()){auto [wall,r,c]=pq.top();pq.pop();for(int k=0;k<4;k++){int nr=r+d[k],nc=c+d[k+1];if(nr<0||nr>=m||nc<0||nc>=n||seen[nr][nc])continue;seen[nr][nc]=1;int nh=g[nr][nc];if(nh<wall){water+=wall-nh;nh=wall;}pq.push({nh,nr,nc});}}return water;}int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;tc++){int m,n;cin>>m>>n;vector<vector<int>>g(m,vector<int>(n));for(auto&r:g)for(int&x:r)cin>>x;if(tc)cout<<"\n\n";cout<<trap(g);}}
