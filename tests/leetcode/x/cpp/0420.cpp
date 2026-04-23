#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>
#include <vector>
using namespace std;int check(string s){int n=s.size();bool lo=0,up=0,dig=0;for(char c:s){lo|=islower((unsigned char)c);up|=isupper((unsigned char)c);dig|=isdigit((unsigned char)c);}int missing=!lo+!up+!dig;vector<int>runs;for(int i=0;i<n;){int j=i;while(j<n&&s[j]==s[i])j++;if(j-i>=3)runs.push_back(j-i);i=j;}if(n<6)return max(missing,6-n);int rep=0;for(int r:runs)rep+=r/3;if(n<=20)return max(missing,rep);int del=n-20,rem=del;sort(runs.begin(),runs.end(),[](int a,int b){return a%3<b%3;});for(int&i:runs){if(rem<=0)break;int use=min(rem,i%3+1);i-=use;rem-=use;}if(rem>0)for(int&i:runs){if(rem<=0)break;int use=min(rem,max(0,i-2));i-=use;rem-=use;}rep=0;for(int r:runs)rep+=max(0,r)/3;return del+max(missing,rep);}int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;tc++){string s;cin>>s;if(tc)cout<<"\n\n";cout<<check(s);}}
