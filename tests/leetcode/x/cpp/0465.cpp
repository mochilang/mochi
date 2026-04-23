#include <iostream>
#include <array>
#include <functional>
#include <unordered_map>
#include <unordered_set>
#include <vector>
using namespace std;int solve(vector<array<int,3>>&trans){unordered_map<int,int>bal;for(auto&t:trans){bal[t[0]]-=t[2];bal[t[1]]+=t[2];}vector<int>d;for(auto&[_,v]:bal)if(v)d.push_back(v);function<int(int)>dfs=[&](int i){while(i<d.size()&&d[i]==0)i++;if(i==d.size())return 0;int best=1e9;unordered_set<int>seen;for(int j=i+1;j<d.size();j++)if(d[i]*d[j]<0&&!seen.count(d[j])){seen.insert(d[j]);d[j]+=d[i];best=min(best,1+dfs(i+1));d[j]-=d[i];if(d[j]+d[i]==0)break;}return best;};return dfs(0);}int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;tc++){int m;cin>>m;vector<array<int,3>>trans(m);for(auto&x:trans)cin>>x[0]>>x[1]>>x[2];if(tc)cout<<"\n\n";cout<<solve(trans);}}
