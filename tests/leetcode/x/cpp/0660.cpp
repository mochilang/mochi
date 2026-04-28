#include <iostream>
#include <string>
#include <algorithm>
using namespace std;
int solve(int n){
    if(n==0) return 0;
    string s;
    while(n){ s.push_back(char('0'+n%9)); n/=9; }
    reverse(s.begin(), s.end());
    return stoi(s);
}
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if(!(cin>>t)) return 0;
    for(int i=0;i<t;i++){
        int n; cin>>n;
        if(i) cout<<"\n\n";
        cout<<solve(n);
    }
}
