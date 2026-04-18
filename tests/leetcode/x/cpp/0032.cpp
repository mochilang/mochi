#include <iostream>
#include <vector>
using namespace std;
int solveCase(const string& s){ vector<int> st{ -1 }; int best=0; for(int i=0;i<(int)s.size();++i){ if(s[i]=='(') st.push_back(i); else { st.pop_back(); if(st.empty()) st.push_back(i); else best=max(best, i-st.back()); } } return best; }
int main(){ int t; if(!(cin>>t)) return 0; for(int tc=0; tc<t; ++tc){ int n; string s=""; cin>>n; if(n>0) cin>>s; cout<<solveCase(s); if(tc+1<t) cout<<'\n'; } }
