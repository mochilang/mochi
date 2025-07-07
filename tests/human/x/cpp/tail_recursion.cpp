#include <iostream>

int sum_rec(int n, int acc) {
    if (n == 0) {
        return acc;
    }
    return sum_rec(n - 1, acc + n);
}

int main() {
    std::cout << sum_rec(10, 0) << std::endl;
    return 0;
}
