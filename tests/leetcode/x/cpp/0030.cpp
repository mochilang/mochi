#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
using namespace std;
vector<int> solveCase(string s, vector<string> words){ vector<int> ans; if(words.empty()) return ans; int wlen=words[0].size(), total=wlen*words.size(); sort(words.begin(), words.end()); for(int i=0;i+total<=(int)s.size();i++){ vector<string> parts; for(int j=0;j<(int)words.size();j++) parts.push_back(s.substr(i+j*wlen,wlen)); sort(parts.begin(), parts.end()); if(parts==words) ans.push_back(i);} return ans; }
int main(){ int t; if(!(cin>>t)) return 0; for(int tc=0; tc<t; ++tc){ string s; int m; cin>>s>>m; vector<string> words(m); for(int i=0;i<m;i++) cin>>words[i]; auto ans=solveCase(s,words); cout<<'['; for(size_t i=0;i<ans.size();++i){ if(i) cout<<','; cout<<ans[i]; } cout<<']'; if(tc+1<t) cout<<'\n'; } }
