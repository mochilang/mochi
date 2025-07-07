#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
struct Nation{int key;std::string name;};
struct Customer{int key;std::string name;double acctbal;int nationkey;std::string address;std::string phone;std::string comment;};
struct Order{int key;int custkey;std::string date;};
struct LineItem{int orderkey;std::string returnflag;double extendedprice;double discount;};
int main(){
    std::vector<Nation> nation={{1,"BRAZIL"}};
    std::vector<Customer> customer={{1,"Alice",100.0,1,"123 St","123-456","Loyal"}};
    std::vector<Order> orders={{1000,1,"1993-10-15"},{2000,1,"1994-01-02"}};
    std::vector<LineItem> lineitem={{1000,"R",1000.0,0.1},{2000,"N",500.0,0.0}};
    std::string start="1993-10-01";std::string end="1994-01-01";
    struct Row{Customer c;LineItem l;};
    std::map<int,double> revenue;std::map<int,Customer> cinfo;
    for(const auto&o:orders){
        if(o.date>=start && o.date<end){
            for(const auto&l:lineitem){
                if(l.orderkey==o.key && l.returnflag=="R"){
                    for(const auto&c:customer){
                        if(c.key==o.custkey){
                            for(const auto&n:nation){if(n.key==c.nationkey){
                                double rev=l.extendedprice*(1-l.discount);
                                revenue[c.key]+=rev; cinfo[c.key]=c;}}
                        }
                    }
                }
            }
        }
    }
    std::vector<std::pair<int,double>> sorted(revenue.begin(),revenue.end());
    std::sort(sorted.begin(),sorted.end(),[](auto&a,auto&b){return a.second>b.second;});
    for(const auto&p:sorted){
        const auto&c=cinfo[p.first];
        std::cout<<c.key<<" "<<c.name<<" revenue="<<p.second<<"\n";
    }
    return 0;
}
