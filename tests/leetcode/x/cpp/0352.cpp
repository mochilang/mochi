#include <iostream>
#include <set>
#include <string>
#include <vector>
using namespace std;
string snapshot(const set<int>& seen){string s="["; bool first=true; int start=0,prev=0; bool open=false; auto flush=[&](){if(!first)s+=","; first=false; s+="["+to_string(start)+","+to_string(prev)+"]";}; for(int v:seen){if(!open){start=prev=v; open=true;}else if(v==prev+1){prev=v;}else{flush(); start=prev=v;}} if(open)flush(); return s+"]";}
int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;vector<string> cases;for(int tc=0;tc<t;++tc){int ops;cin>>ops;set<int> seen;vector<string> snaps;for(int i=0;i<ops;++i){string op;cin>>op;if(op=="A"){int v;cin>>v;seen.insert(v);}else snaps.push_back(snapshot(seen));}string s="[";for(int i=0;i<(int)snaps.size();++i){if(i)s+=",";s+=snaps[i];}cases.push_back(s+"]");}for(int i=0;i<(int)cases.size();++i){if(i)cout<<"\n\n";cout<<cases[i];}}
