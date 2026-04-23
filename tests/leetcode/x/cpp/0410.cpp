#include <iostream>
#include <vector>
using namespace std;int splitArray(vector<int>&a,int k){long long lo=0,hi=0;for(int x:a){lo=max(lo,(long long)x);hi+=x;}while(lo<hi){long long mid=(lo+hi)/2,cur=0;int pieces=1;for(int x:a){if(cur+x>mid){pieces++;cur=x;}else cur+=x;}if(pieces<=k)hi=mid;else lo=mid+1;}return (int)lo;}int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;tc++){int n,k;cin>>n>>k;vector<int>a(n);for(int&i:a)cin>>i;if(tc)cout<<"\n\n";cout<<splitArray(a,k);}}
