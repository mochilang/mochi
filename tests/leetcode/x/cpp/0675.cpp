#include <iostream>
#include <queue>
#include <tuple>
#include <vector>
#include <algorithm>
using namespace std;
int bfs(const vector<vector<int>>& forest,int sr,int sc,int tr,int tc){
    if(sr==tr&&sc==tc) return 0;
    int m=forest.size(), n=forest[0].size();
    queue<tuple<int,int,int>> q;
    vector<vector<int>> seen(m, vector<int>(n));
    q.push({sr,sc,0}); seen[sr][sc]=1;
    while(!q.empty()){
        auto [r,c,d]=q.front(); q.pop();
        for(auto [dr,dc] : vector<pair<int,int>>{{1,0},{-1,0},{0,1},{0,-1}}){
            int nr=r+dr,nc=c+dc;
            if(0<=nr&&nr<m&&0<=nc&&nc<n&&forest[nr][nc]!=0&&!seen[nr][nc]){
                if(nr==tr&&nc==tc) return d+1;
                seen[nr][nc]=1; q.push({nr,nc,d+1});
            }
        }
    }
    return -1;
}
int solve(const vector<vector<int>>& forest){
    vector<tuple<int,int,int>> trees;
    for(int i=0;i<(int)forest.size();i++) for(int j=0;j<(int)forest[0].size();j++) if(forest[i][j]>1) trees.push_back({forest[i][j],i,j});
    sort(trees.begin(), trees.end());
    int sr=0, sc=0, total=0;
    for(auto [h,tr,tc] : trees){
        int d=bfs(forest,sr,sc,tr,tc);
        if(d<0) return -1;
        total+=d; sr=tr; sc=tc;
    }
    return total;
}
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if(!(cin>>t)) return 0;
    for(int tc=0;tc<t;tc++){
        int m,n; cin>>m>>n;
        vector<vector<int>> forest(m, vector<int>(n));
        for(int i=0;i<m;i++) for(int j=0;j<n;j++) cin>>forest[i][j];
        if(tc) cout<<"\n\n";
        cout<<solve(forest);
    }
}
