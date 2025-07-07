#include <iostream>
#include <set>
#include <vector>
int main(){
    std::set<int> a={1,2};
    std::set<int> b={2,3};
    std::set<int> union_ab=a; union_ab.insert(b.begin(),b.end());
    for(int x:union_ab) std::cout<<x<<" "; std::cout<<"\n";
    std::set<int> ex={1,2,3}; for(int x:{2}) ex.erase(x); for(int x:ex) std::cout<<x<<" "; std::cout<<"\n";
    std::set<int> inter; for(int x:{1,2,3}) if(b.count(x)) inter.insert(x); for(int x:inter) std::cout<<x<<" "; std::cout<<"\n";
    std::vector<int> union_all={1,2,2,3}; std::cout<<union_all.size()<<std::endl;
    return 0;
}
