#include <algorithm>
#include <climits>
#include <iostream>
#include <set>
#include <vector>
using namespace std;int solve(vector<vector<int>>&m,int k){int R=m.size(),C=m[0].size(),best=INT_MIN;for(int top=0;top<R;++top){vector<int>s(C);for(int bot=top;bot<R;++bot){for(int c=0;c<C;++c)s[c]+=m[bot][c];set<int>pre{0};int cur=0;for(int x:s){cur+=x;auto it=pre.lower_bound(cur-k);if(it!=pre.end())best=max(best,cur-*it);pre.insert(cur);}}}return best;}int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;++tc){int r,c;cin>>r>>c;vector<vector<int>>m(r,vector<int>(c));for(auto&row:m)for(int&x:row)cin>>x;int k;cin>>k;if(tc)cout<<"\n\n";cout<<solve(m,k);}}
