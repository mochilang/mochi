#include <iostream>
#include <functional>
#include <string>
#include <unordered_map>
#include <vector>
using namespace std;vector<vector<string>> solve(vector<string>&words){if(words.empty())return{};int n=words[0].size();unordered_map<string,vector<string>> pref;for(auto&w:words)for(int i=0;i<=n;i++)pref[w.substr(0,i)].push_back(w);vector<vector<string>>ans;vector<string>sq;function<void()>dfs=[&](){int d=sq.size();if(d==n){ans.push_back(sq);return;}string p;for(auto&r:sq)p+=r[d];for(auto&w:pref[p]){sq.push_back(w);dfs();sq.pop_back();}};for(auto&w:words){sq={w};dfs();}return ans;}string fmt(vector<vector<string>>&ans){string out="[";for(size_t i=0;i<ans.size();i++){if(i)out+=",";out+="[";for(size_t j=0;j<ans[i].size();j++){if(j)out+=",";out+="\""+ans[i][j]+"\"";}out+="]";}return out+"]";}int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;tc++){int n;cin>>n;vector<string>w(n);for(auto&s:w)cin>>s;if(tc)cout<<"\n\n";auto ans=solve(w);cout<<fmt(ans);}}
