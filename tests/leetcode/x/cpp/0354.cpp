#include <algorithm>
#include <iostream>
#include <vector>
using namespace std;
int maxEnvelopes(vector<pair<int,int>>& e){sort(e.begin(),e.end(),[](auto&a,auto&b){return a.first==b.first?a.second>b.second:a.first<b.first;});vector<int> tails;for(auto&p:e){auto it=lower_bound(tails.begin(),tails.end(),p.second);if(it==tails.end())tails.push_back(p.second);else *it=p.second;}return tails.size();}
int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;++tc){int n;cin>>n;vector<pair<int,int>> e(n);for(auto&p:e)cin>>p.first>>p.second;if(tc)cout<<"\n\n";cout<<maxEnvelopes(e);}}
