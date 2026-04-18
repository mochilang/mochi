#include <iostream>
#include <string>
using namespace std;
bool isMatch(const string& s,const string& p){int i=0,j=0,star=-1,match=0;while(i<(int)s.size()){if(j<(int)p.size()&&(p[j]=='?'||p[j]==s[i])){i++;j++;}else if(j<(int)p.size()&&p[j]=='*'){star=j;match=i;j++;}else if(star!=-1){j=star+1;match++;i=match;}else return false;}while(j<(int)p.size()&&p[j]=='*')j++;return j==(int)p.size();}
int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;string dummy;getline(cin,dummy);for(int tc=0;tc<t;tc++){int n,m;cin>>n;getline(cin,dummy);string s="",p="";if(n>0)getline(cin,s);cin>>m;getline(cin,dummy);if(m>0)getline(cin,p);if(tc)cout<<'\n';cout<<(isMatch(s,p)?"true":"false");}return 0;}
