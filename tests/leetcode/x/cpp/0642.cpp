#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <algorithm>
using namespace std;
struct AutocompleteSystem{
    unordered_map<string,int> counts;
    string current;
    AutocompleteSystem(const vector<string>& s,const vector<int>& t){for(int i=0;i<(int)s.size();i++)counts[s[i]]+=t[i];}
    vector<string> input(const string& ch){
        if(ch=="#"){counts[current]++;current.clear();return {};}
        current+=ch;
        vector<string> matches;
        for(auto& [s,c]:counts) if(s.rfind(current,0)==0) matches.push_back(s);
        sort(matches.begin(),matches.end(),[&](const string& a,const string& b){ if(counts[a]!=counts[b]) return counts[a]>counts[b]; return a<b; });
        if(matches.size()>3) matches.resize(3);
        return matches;
    }
};
string formatList(const vector<string>* items){
    if(!items) return "null";
    string out="[";
    for(int i=0;i<(int)items->size();i++){ if(i) out+=","; out+="\""+(*items)[i]+"\""; }
    return out+"]";
}
string decodeChar(const string& s){ return s=="<space>" ? " " : s; }
int main(){
    vector<string> lines; string line;
    while(getline(cin,line)){ if(!line.empty() && line.back()=='\r') line.pop_back(); if(!line.empty()) lines.push_back(line); }
    int t=stoi(lines[0]), idx=1;
    vector<string> cases;
    for(int tc=0;tc<t;tc++){
        int q=stoi(lines[idx++]);
        vector<string> out;
        AutocompleteSystem* sys=nullptr;
        for(int i=0;i<q;i++){
            string cur=lines[idx++];
            if(cur.rfind("C ",0)==0){
                int n=stoi(cur.substr(2));
                vector<string> s(n); vector<int> tm(n);
                for(int j=0;j<n;j++){ auto p=lines[idx++].find('|'); tm[j]=stoi(lines[idx-1].substr(0,p)); s[j]=lines[idx-1].substr(p+1); }
                delete sys; sys=new AutocompleteSystem(s,tm); out.push_back("null");
            }else{
                string token=cur.substr(2);
                auto ans=sys->input(decodeChar(token));
                out.push_back(formatList(&ans));
            }
        }
        delete sys;
        string s="[";
        for(int i=0;i<(int)out.size();i++){ if(i) s+=","; s+=out[i]; }
        cases.push_back(s+"]");
    }
    for(int i=0;i<(int)cases.size();i++){ if(i) cout<<"\n\n"; cout<<cases[i]; }
}
