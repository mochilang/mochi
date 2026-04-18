#include <iostream>
#include <string>
#include <climits>
#include <cctype>
using namespace std;

static int myAtoi(const string& s) {
    int i = 0;
    while (i < (int)s.size() && s[i] == ' ') i++;
    int sign = 1;
    if (i < (int)s.size() && (s[i] == '+' || s[i] == '-')) {
        if (s[i] == '-') sign = -1;
        i++;
    }
    int ans = 0;
    int limit = sign > 0 ? 7 : 8;
    while (i < (int)s.size() && isdigit((unsigned char)s[i])) {
        int digit = s[i] - '0';
        if (ans > 214748364 || (ans == 214748364 && digit > limit)) return sign > 0 ? INT_MAX : INT_MIN;
        ans = ans * 10 + digit;
        i++;
    }
    return sign * ans;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    string s;
    getline(cin, s);
    for (int i = 0; i < t; i++) {
        getline(cin, s);
        cout << myAtoi(s);
        if (i + 1 < t) cout << '\n';
    }
    return 0;
}
