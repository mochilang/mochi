#include <iostream>
#include <vector>
int main() {
    std::vector<int> a = {1,2};
    a.push_back(3);
    for (size_t i=0;i<a.size();++i) {
        if (i) std::cout << ' ';
        std::cout << a[i];
    }
    return 0;
}
