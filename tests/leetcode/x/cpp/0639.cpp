#include <iostream>
#include <string>
#include <vector>
using namespace std;
const long long MOD = 1000000007LL;
long long one(char c){ if(c=='*') return 9; if(c=='0') return 0; return 1; }
long long two(char a,char b){
    if(a=='*'&&b=='*') return 15;
    if(a=='*') return b<='6'?2:1;
    if(b=='*'){ if(a=='1') return 9; if(a=='2') return 6; return 0; }
    int v=(a-'0')*10+(b-'0');
    return 10<=v&&v<=26;
}
long long solve(const string& s){
    long long prev2=1, prev1=one(s[0]);
    for(int i=1;i<(int)s.size();i++){
        long long cur=(one(s[i])*prev1 + two(s[i-1],s[i])*prev2)%MOD;
        prev2=prev1; prev1=cur;
    }
    return prev1;
}
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if(!(cin>>t)) return 0;
    vector<string> ans;
    for(int i=0;i<t;i++){ string s; cin>>s; ans.push_back(to_string(solve(s))); }
    for(int i=0;i<t;i++){ if(i) cout<<"\n\n"; cout<<ans[i]; }
}
