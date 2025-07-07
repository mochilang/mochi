#include <iostream>
#include <vector>
#include <string>
int main(){
    std::vector<int> nums={1,2,3};
    std::vector<std::string> letters={"A","B"};
    std::cout << "--- Even pairs ---\n";
    for(int n:nums){
        if(n%2==0){
            for(const auto &l:letters){
                std::cout<<n<<" "<<l<<"\n";
            }
        }
    }
    return 0;
}
