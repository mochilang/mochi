#include <iostream>
using namespace std;int pigs(int b,int d,int t){int states=t/d+1,p=0,cap=1;while(cap<b){p++;cap*=states;}return p;}int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int tc;if(!(cin>>tc))return 0;for(int i=0;i<tc;i++){int b,d,t;cin>>b>>d>>t;if(i)cout<<"\n\n";cout<<pigs(b,d,t);}}
