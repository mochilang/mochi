#include <iostream>
#include <string>

using namespace std;

static long long count_digit_one(long long n) {
    long long total = 0;
    for (long long m = 1; m <= n; m *= 10) {
        long long high = n / (m * 10);
        long long cur = (n / m) % 10;
        long long low = n % m;
        if (cur == 0) total += high * m;
        else if (cur == 1) total += high * m + low + 1;
        else total += (high + 1) * m;
    }
    return total;
}

int main() {
    string line;
    if (!getline(cin, line)) return 0;
    int t = stoi(line);
    for (int i = 0; i < t; ++i) {
        getline(cin, line);
        if (i) cout << '\n';
        cout << count_digit_one(stoll(line));
    }
    return 0;
}
