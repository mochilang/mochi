#include <iostream>
#include <queue>
#include <tuple>
#include <vector>
using namespace std;
pair<int,int> smallestRange(const vector<vector<int>>& nums){
    using T=tuple<int,int,int>;
    priority_queue<T,vector<T>,greater<T>> pq;
    int currentMax=-1000000007;
    for(int i=0;i<(int)nums.size();i++){pq.push({nums[i][0],i,0});currentMax=max(currentMax,nums[i][0]);}
    int bestL=get<0>(pq.top()),bestR=currentMax;
    while(true){
        auto [cur,row,idx]=pq.top();pq.pop();
        if(currentMax-cur<bestR-bestL||(currentMax-cur==bestR-bestL&&cur<bestL)){bestL=cur;bestR=currentMax;}
        if(idx+1==(int)nums[row].size()) return {bestL,bestR};
        int nv=nums[row][idx+1];
        pq.push({nv,row,idx+1});
        currentMax=max(currentMax,nv);
    }
}
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if(!(cin>>t)) return 0;
    for(int tc=0;tc<t;tc++){
        int k; cin>>k;
        vector<vector<int>> nums(k);
        for(int i=0;i<k;i++){
            int n; cin>>n;
            nums[i].resize(n);
            for(int j=0;j<n;j++) cin>>nums[i][j];
        }
        auto [l,r]=smallestRange(nums);
        if(tc) cout<<"\n\n";
        cout<<"["<<l<<","<<r<<"]";
    }
}
