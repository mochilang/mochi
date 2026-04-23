#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
using namespace std;struct RC{vector<int> vals;unordered_map<int,unordered_set<int>> pos;bool ins(int v){bool fresh=pos[v].empty();pos[v].insert(vals.size());vals.push_back(v);return fresh;}bool rem(int v){if(pos[v].empty())return false;int idx=*pos[v].begin();for(int x:pos[v])idx=max(idx,x);pos[v].erase(idx);int last=vals.back(),li=vals.size()-1;if(idx!=li){vals[idx]=last;pos[last].erase(li);pos[last].insert(idx);}vals.pop_back();return true;}};int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;++tc){int ops;cin>>ops;RC rc;vector<string> out;for(int i=0;i<ops;++i){string op;cin>>op;if(op=="C")out.push_back("null");else if(op=="I"){int v;cin>>v;out.push_back(rc.ins(v)?"true":"false");}else if(op=="R"){int v;cin>>v;out.push_back(rc.rem(v)?"true":"false");}else out.push_back(to_string(rc.vals[0]));}if(tc)cout<<"\n\n";cout<<"[";for(int i=0;i<(int)out.size();++i){if(i)cout<<",";cout<<out[i];}cout<<"]";}}
