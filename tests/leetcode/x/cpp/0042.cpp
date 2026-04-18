#include <iostream>
#include <vector>
using namespace std;
int trap(vector<int>& h){int left=0,right=(int)h.size()-1,leftMax=0,rightMax=0,water=0;while(left<=right){if(leftMax<=rightMax){if(h[left]<leftMax)water+=leftMax-h[left];else leftMax=h[left];left++;}else{if(h[right]<rightMax)water+=rightMax-h[right];else rightMax=h[right];right--;}}return water;}
int main(){ios::sync_with_stdio(false);cin.tie(nullptr);int t;if(!(cin>>t))return 0;for(int tc=0;tc<t;tc++){int n;cin>>n;vector<int>a(n);for(int i=0;i<n;i++)cin>>a[i];if(tc)cout<<'\n';cout<<trap(a);}return 0;}
