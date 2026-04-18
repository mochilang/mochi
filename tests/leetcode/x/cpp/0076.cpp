#include <iostream>
#include <string>
using namespace std;
string minWindow(string s, string t){ int need[128]={0}; int missing=t.size(); for(char c:t) need[(unsigned char)c]++; int left=0,bestStart=0,bestLen=s.size()+1; for(int right=0; right<(int)s.size(); right++){ unsigned char c=s[right]; if(need[c]>0) missing--; need[c]--; while(missing==0){ if(right-left+1<bestLen){ bestStart=left; bestLen=right-left+1; } unsigned char lc=s[left]; need[lc]++; if(need[lc]>0) missing++; left++; } } return bestLen>(int)s.size()?"":s.substr(bestStart,bestLen); }
int main(){ ios::sync_with_stdio(false); cin.tie(nullptr); int t; string s,p; if(!(cin>>t)) return 0; getline(cin,s); for(int i=0;i<t;i++){ getline(cin,s); getline(cin,p); cout<<minWindow(s,p); if(i+1<t) cout<<'\n'; } }
