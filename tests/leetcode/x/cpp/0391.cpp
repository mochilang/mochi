#include <iostream>
#include <set>

#include <array>
#include <vector>
#include <climits>
using namespace std;void tog(set<pair<int,int>>&s,pair<int,int>p){auto it=s.find(p);if(it==s.end())s.insert(p);else s.erase(it);}bool cover(vector<array<int,4>>&rs){long long area=0;int minx=INT_MAX,miny=INT_MAX,maxx=INT_MIN,maxy=INT_MIN;set<pair<int,int>> c;for(auto&r:rs){int x1=r[0],y1=r[1],x2=r[2],y2=r[3];minx=min(minx,x1);miny=min(miny,y1);maxx=max(maxx,x2);maxy=max(maxy,y2);area+=1LL*(x2-x1)*(y2-y1);tog(c,{x1,y1});tog(c,{x1,y2});tog(c,{x2,y1});tog(c,{x2,y2});}set<pair<int,int>> outer{{minx,miny},{minx,maxy},{maxx,miny},{maxx,maxy}};return area==1LL*(maxx-minx)*(maxy-miny)&&c==outer;}int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;++tc){int n;cin>>n;vector<array<int,4>>rs(n);for(auto&r:rs)cin>>r[0]>>r[1]>>r[2]>>r[3];if(tc)cout<<"\n\n";cout<<(cover(rs)?"true":"false");}}
