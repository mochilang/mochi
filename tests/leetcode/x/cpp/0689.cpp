#include <iostream>
#include <string>
#include <vector>
using namespace std;
vector<int> solve(const vector<int>& nums,int k){
    int n=nums.size(), m=n-k+1;
    vector<int> w(m);
    int sum=0;
    for(int i=0;i<n;i++){
        sum+=nums[i];
        if(i>=k) sum-=nums[i-k];
        if(i>=k-1) w[i-k+1]=sum;
    }
    vector<int> left(m), right(m);
    int best=0;
    for(int i=0;i<m;i++){ if(w[i]>w[best]) best=i; left[i]=best; }
    best=m-1;
    for(int i=m-1;i>=0;i--){ if(w[i]>=w[best]) best=i; right[i]=best; }
    vector<int> ans{0,0,0}; int bestSum=-1;
    for(int mid=k; mid<m-k; mid++){
        int a=left[mid-k], c=right[mid+k];
        int total=w[a]+w[mid]+w[c];
        vector<int> cand{a,mid,c};
        if(total>bestSum || total==bestSum && cand<ans){ bestSum=total; ans=cand; }
    }
    return ans;
}
string fmt(const vector<int>& a){ return "["+to_string(a[0])+","+to_string(a[1])+","+to_string(a[2])+"]"; }
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if(!(cin>>t)) return 0;
    for(int tc=0;tc<t;tc++){
        int n,k; cin>>n>>k;
        vector<int> nums(n); for(int i=0;i<n;i++) cin>>nums[i];
        if(tc) cout<<"\n\n";
        cout<<fmt(solve(nums,k));
    }
}
