#include <iostream>
#include <string>
using namespace std;

static int value(char c) {
    switch (c) {
        case 'I': return 1; case 'V': return 5; case 'X': return 10;
        case 'L': return 50; case 'C': return 100; case 'D': return 500; case 'M': return 1000;
    }
    return 0;
}

static int romanToInt(const string& s) {
    int total = 0;
    for (int i = 0; i < (int)s.size(); i++) {
        int cur = value(s[i]);
        int next = i + 1 < (int)s.size() ? value(s[i + 1]) : 0;
        total += cur < next ? -cur : cur;
    }
    return total;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    string s;
    while (t--) {
        cin >> s;
        cout << romanToInt(s) << "\n";
    }
    return 0;
}
