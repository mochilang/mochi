#include <iostream>
#include <string>
#include <vector>
using namespace std;
string rearrange(string s,int k){if(k<=1)return s;vector<int>cnt(26),last(26,-1000000000);for(char c:s)cnt[c-'a']++;string out;for(int pos=0;pos<(int)s.size();++pos){int best=-1;for(int i=0;i<26;++i)if(cnt[i]>0&&pos-last[i]>=k&&(best<0||cnt[i]>cnt[best]))best=i;if(best<0)return "";out.push_back('a'+best);cnt[best]--;last[best]=pos;}return out;}
int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;++tc){string s;int k;cin>>s>>k;if(tc)cout<<"\n\n";cout<<'"'<<rearrange(s,k)<<'"';}}
