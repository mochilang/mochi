#include <iostream>
#include <vector>
#include <string>
using namespace std;
vector<string> justify(vector<string> words,int maxWidth){ vector<string> res; int i=0; while(i<(int)words.size()){ int j=i,total=0; while(j<(int)words.size() && total+(int)words[j].size()+(j-i)<=maxWidth){ total+=words[j].size(); j++; } int gaps=j-i-1; string line; if(j==(int)words.size()||gaps==0){ for(int k=i;k<j;k++){ if(k>i) line+=' '; line+=words[k]; } line+=string(maxWidth-line.size(),' '); } else { int spaces=maxWidth-total, base=spaces/gaps, extra=spaces%gaps; for(int k=i;k<j-1;k++){ line+=words[k]; line+=string(base+((k-i<extra)?1:0),' '); } line+=words[j-1]; } res.push_back(line); i=j; } return res; }
int main(){ ios::sync_with_stdio(false); cin.tie(nullptr); int t; if(!(cin>>t)) return 0; string s; getline(cin,s); for(int tc=0; tc<t; tc++){ getline(cin,s); int n=stoi(s); vector<string> words(n); for(int i=0;i<n;i++) getline(cin,words[i]); getline(cin,s); int width=stoi(s); auto ans=justify(words,width); cout<<ans.size()<<'\n'; for(auto &x: ans) cout<<'|'<<x<<"|\n"; if(tc+1<t) cout<<"=\n"; } }
