#include <iostream>
#include <vector>
#include <algorithm>
struct Data{int a;int b;};
int main(){
    std::vector<Data> data={{1,2},{1,1},{0,5}};
    std::sort(data.begin(),data.end(),[](const Data&x,const Data&y){return (x.a!=y.a)?x.a<y.a:x.b<y.b;});
    for(const auto&d:data) std::cout<<"{"<<d.a<<","<<d.b<<"}"<<" ";
    std::cout<<std::endl;return 0;
}
