#include <iostream>
#include <limits>
#include <string>
#include <vector>
using namespace std;
bool lessPath(const vector<int>& a,const vector<int>& b){ if(b.empty()) return true; return a<b; }
vector<int> solve(const vector<int>& coins,int maxJump){
    int n=coins.size(), INF=numeric_limits<int>::max()/4;
    vector<int> dp(n,INF);
    vector<vector<int>> path(n);
    if(coins[n-1]==-1) return {};
    dp[n-1]=coins[n-1];
    path[n-1]={n};
    for(int i=n-2;i>=0;i--){
        if(coins[i]==-1) continue;
        int best=INF; vector<int> bestPath;
        for(int j=i+1;j<n&&j<=i+maxJump;j++){
            if(dp[j]>=INF||path[j].empty()) continue;
            int cost=coins[i]+dp[j];
            vector<int> cand{ i+1 };
            cand.insert(cand.end(), path[j].begin(), path[j].end());
            if(cost<best || cost==best && lessPath(cand,bestPath)){ best=cost; bestPath=cand; }
        }
        dp[i]=best; path[i]=bestPath;
    }
    return path[0];
}
string fmtPath(const vector<int>& path){
    string out="[";
    for(int i=0;i<(int)path.size();i++){ if(i) out+=","; out+=to_string(path[i]); }
    return out+"]";
}
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if(!(cin>>t)) return 0;
    for(int tc=0;tc<t;tc++){
        int n,maxJump; cin>>n>>maxJump;
        vector<int> coins(n);
        for(int i=0;i<n;i++) cin>>coins[i];
        if(tc) cout<<"\n\n";
        cout<<fmtPath(solve(coins,maxJump));
    }
}
