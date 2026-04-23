#include <iostream>
#include <unordered_map>
#include <vector>
using namespace std;long long count(vector<long long>&a){vector<unordered_map<long long,long long>>dp(a.size());long long ans=0;for(int i=0;i<a.size();i++)for(int j=0;j<i;j++){long long d=a[i]-a[j],prev=dp[j][d];ans+=prev;dp[i][d]+=prev+1;}return ans;}int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;tc++){int n;cin>>n;vector<long long>a(n);for(auto&x:a)cin>>x;if(tc)cout<<"\n\n";cout<<count(a);}}
