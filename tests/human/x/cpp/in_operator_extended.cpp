#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>
int main(){
    std::vector<int> xs={1,2,3};
    std::vector<int> ys; for(int x:xs) if(x%2==1) ys.push_back(x);
    auto contains=[&](int val){return std::find(ys.begin(),ys.end(),val)!=ys.end();};
    std::cout<<(contains(1)?"true":"false")<<std::endl;
    std::cout<<(contains(2)?"true":"false")<<std::endl;
    std::map<std::string,int> m={{"a",1}};
    std::cout<<(m.count("a")?"true":"false")<<std::endl;
    std::cout<<(m.count("b")?"true":"false")<<std::endl;
    std::string s="hello";
    std::cout<<(s.find("ell")!=std::string::npos?"true":"false")<<std::endl;
    std::cout<<(s.find("foo")!=std::string::npos?"true":"false")<<std::endl;
    return 0;
}
