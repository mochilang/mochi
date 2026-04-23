#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <vector>
using namespace std;bool canCross(vector<int>&s){unordered_map<int,int>pos;for(int i=0;i<(int)s.size();++i)pos[s[i]]=i;vector<unordered_set<int>>dp(s.size());dp[0].insert(0);int last=s.size()-1;for(int i=0;i<(int)s.size();++i){for(int k:dp[i])for(int step:{k-1,k,k+1}){if(step<=0)continue;auto it=pos.find(s[i]+step);if(it==pos.end())continue;if(it->second==last)return true;dp[it->second].insert(step);}}return last==0;}int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;++tc){int n;cin>>n;vector<int>s(n);for(int&i:s)cin>>i;if(tc)cout<<"\n\n";cout<<(canCross(s)?"true":"false");}}
