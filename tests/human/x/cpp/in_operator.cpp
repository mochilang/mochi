#include <iostream>
#include <vector>
#include <algorithm>
int main(){
    std::vector<int> xs{1,2,3};
    bool has2 = std::find(xs.begin(), xs.end(), 2) != xs.end();
    bool has5 = std::find(xs.begin(), xs.end(), 5) != xs.end();
    std::cout << (has2?"true":"false") << std::endl;
    std::cout << (!has5?"true":"false");
    return 0;
}
