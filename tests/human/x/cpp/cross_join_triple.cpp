#include <iostream>
#include <vector>
#include <string>
int main(){
    std::vector<int> nums={1,2};
    std::vector<std::string> letters={"A","B"};
    std::vector<bool> bools={true,false};
    std::cout << "--- Cross Join of three lists ---\n";
    for(int n:nums){
        for(const auto &l:letters){
            for(bool b:bools){
                std::cout<<n<<" "<<l<<" "<<(b?"true":"false")<<"\n";
            }
        }
    }
    return 0;
}
