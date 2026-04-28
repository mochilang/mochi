#include <iostream>
#include <vector>
#include <climits>
using namespace std;
int solve(const vector<int>& bulbs,int k){
    int n=bulbs.size();
    vector<int> days(n);
    for(int i=0;i<n;i++) days[bulbs[i]-1]=i+1;
    int ans=INT_MAX/4, left=0, right=k+1;
    while(right<n){
        bool valid=true;
        for(int i=left+1;i<right;i++){
            if(days[i] < max(days[left], days[right])){
                left=i; right=i+k+1; valid=false; break;
            }
        }
        if(valid){
            ans=min(ans, max(days[left], days[right]));
            left=right; right=left+k+1;
        }
    }
    return ans>=INT_MAX/8 ? -1 : ans;
}
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if(!(cin>>t)) return 0;
    for(int tc=0;tc<t;tc++){
        int n,k; cin>>n>>k;
        vector<int> bulbs(n);
        for(int i=0;i<n;i++) cin>>bulbs[i];
        if(tc) cout<<"\n\n";
        cout<<solve(bulbs,k);
    }
}
