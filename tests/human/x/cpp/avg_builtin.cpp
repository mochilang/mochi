#include <iostream>
#include <vector>
int main(){
    std::vector<int> v = {1,2,3};
    double sum = 0;
    for (auto x: v) sum += x;
    double avg = v.empty()?0:sum/v.size();
    std::cout << avg;
    return 0;
}
