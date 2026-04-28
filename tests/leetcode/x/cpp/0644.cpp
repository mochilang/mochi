#include <iomanip>
#include <iostream>
#include <vector>
using namespace std;
bool feasible(const vector<int>& nums,int k,double mid){
    double total=0.0, prev=0.0, minPrev=0.0;
    for(int i=0;i<(int)nums.size();i++){
        total += nums[i]-mid;
        if(i>=k){
            prev += nums[i-k]-mid;
            if(prev<minPrev) minPrev=prev;
        }
        if(i+1>=k && total-minPrev>=0) return true;
    }
    return false;
}
double solve(const vector<int>& nums,int k){
    double lo=nums[0], hi=nums[0];
    for(int x:nums){ if(x<lo) lo=x; if(x>hi) hi=x; }
    for(int i=0;i<60;i++){
        double mid=(lo+hi)/2.0;
        if(feasible(nums,k,mid)) lo=mid; else hi=mid;
    }
    return lo;
}
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if(!(cin>>t)) return 0;
    cout<<fixed<<setprecision(5);
    for(int tc=0;tc<t;tc++){
        int n,k; cin>>n>>k;
        vector<int> nums(n);
        for(int i=0;i<n;i++) cin>>nums[i];
        if(tc) cout<<"\n\n";
        cout<<solve(nums,k);
    }
}
