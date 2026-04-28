#include <iostream>
#include <string>
#include <vector>
using namespace std;
vector<int> solve(const vector<vector<int>>& edges){
    int n=edges.size();
    vector<int> parentOf(n+1), candA, candB;
    for(auto &e:edges){
        int u=e[0], v=e[1];
        if(parentOf[v]==0) parentOf[v]=u;
        else { candA={parentOf[v],v}; candB={u,v}; break; }
    }
    vector<int> parent(n+1);
    for(int i=0;i<=n;i++) parent[i]=i;
    auto find=[&](int x){ while(parent[x]!=x){ parent[x]=parent[parent[x]]; x=parent[x]; } return x; };
    auto unite=[&](int a,int b){ int ra=find(a), rb=find(b); if(ra==rb) return false; parent[rb]=ra; return true; };
    for(auto &e:edges){
        if(!candB.empty() && e[0]==candB[0] && e[1]==candB[1]) continue;
        if(!unite(e[0],e[1])) return !candA.empty()?candA:e;
    }
    return candB;
}
string fmt(const vector<int>& e){ return "["+to_string(e[0])+","+to_string(e[1])+"]"; }
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if(!(cin>>t)) return 0;
    for(int tc=0;tc<t;tc++){
        int n; cin>>n;
        vector<vector<int>> edges(n, vector<int>(2));
        for(int i=0;i<n;i++) cin>>edges[i][0]>>edges[i][1];
        if(tc) cout<<"\n\n";
        cout<<fmt(solve(edges));
    }
}
