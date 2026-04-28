#include <cmath>
#include <iostream>
#include <vector>
using namespace std;
const double EPS = 1e-6;
bool dfs(vector<double> arr){
    if(arr.size()==1) return fabs(arr[0]-24.0)<EPS;
    int n=arr.size();
    for(int i=0;i<n;i++) for(int j=i+1;j<n;j++){
        vector<double> rest;
        for(int k=0;k<n;k++) if(k!=i&&k!=j) rest.push_back(arr[k]);
        double a=arr[i], b=arr[j];
        vector<double> cands{a+b,a*b,a-b,b-a};
        if(fabs(b)>EPS) cands.push_back(a/b);
        if(fabs(a)>EPS) cands.push_back(b/a);
        for(double v:cands){ auto next=rest; next.push_back(v); if(dfs(next)) return true; }
    }
    return false;
}
bool solve(const vector<int>& cards){ vector<double> arr(cards.begin(), cards.end()); return dfs(arr); }
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if(!(cin>>t)) return 0;
    for(int i=0;i<t;i++){
        vector<int> cards(4); for(int j=0;j<4;j++) cin>>cards[j];
        if(i) cout<<"\n\n";
        cout<<(solve(cards)?"true":"false");
    }
}
