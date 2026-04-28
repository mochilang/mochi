#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
using namespace std;
struct Cell{int r,c;bool operator==(const Cell& o)const{return r==o.r&&c==o.c;}};
struct Hash{size_t operator()(const Cell& x)const{return (size_t)x.r*131u+x.c;}};
struct Entry{int kind=0,val=0;unordered_map<Cell,int,Hash> refs;};
struct Excel{
    unordered_map<Cell,Entry,Hash> cells;
    static Cell parseCell(const string& s){return {stoi(s.substr(1)),s[0]-'A'};}
    void set(int r,char c,int v){cells[{r,c-'A'}]=Entry{0,v,{}};}
    int get(int r,char c){
        Cell k{r,c-'A'};
        auto it=cells.find(k);
        if(it==cells.end())return 0;
        if(it->second.kind==0)return it->second.val;
        int sum=0;
        for(auto&[ref,cnt]:it->second.refs)sum+=get(ref.r,char('A'+ref.c))*cnt;
        return sum;
    }
    int sum(int r,char c,const vector<string>& nums){
        unordered_map<Cell,int,Hash> refs;
        for(auto&s:nums){
            auto pos=s.find(':');
            if(pos==string::npos){refs[parseCell(s)]++;continue;}
            Cell a=parseCell(s.substr(0,pos)),b=parseCell(s.substr(pos+1));
            for(int cc=a.c;cc<=b.c;cc++)for(int rr=a.r;rr<=b.r;rr++)refs[{rr,cc}]++;
        }
        cells[{r,c-'A'}]=Entry{1,0,refs};
        return get(r,c);
    }
};
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if(!(cin>>t))return 0;
    for(int tc=0;tc<t;tc++){
        int q;
        cin>>q;
        Excel ex;
        vector<string> out;
        for(int i=0;i<q;i++){
            string op;
            cin>>op;
            if(op=="C"){
                int h; char w;
                cin>>h>>w;
                ex=Excel();
                out.push_back("null");
            }else if(op=="S"){
                int r,v; char c;
                cin>>r>>c>>v;
                ex.set(r,c,v);
                out.push_back("null");
            }else if(op=="G"){
                int r; char c;
                cin>>r>>c;
                out.push_back(to_string(ex.get(r,c)));
            }else{
                int r,k; char c;
                cin>>r>>c>>k;
                vector<string> nums(k);
                for(int j=0;j<k;j++)cin>>nums[j];
                out.push_back(to_string(ex.sum(r,c,nums)));
            }
        }
        if(tc)cout<<"\n\n";
        cout<<"[";
        for(int i=0;i<out.size();i++){if(i)cout<<",";cout<<out[i];}
        cout<<"]";
    }
}
