#include <iostream>
#include <string>
using namespace std;

static pair<int, int> expand(const string& s, int left, int right) {
    while (left >= 0 && right < (int)s.size() && s[left] == s[right]) {
        left--;
        right++;
    }
    return {left + 1, right - left - 1};
}

static string longestPalindrome(const string& s) {
    int bestStart = 0;
    int bestLen = s.empty() ? 0 : 1;
    for (int i = 0; i < (int)s.size(); i++) {
        auto [start1, len1] = expand(s, i, i);
        if (len1 > bestLen) {
            bestStart = start1;
            bestLen = len1;
        }
        auto [start2, len2] = expand(s, i, i + 1);
        if (len2 > bestLen) {
            bestStart = start2;
            bestLen = len2;
        }
    }
    return s.substr(bestStart, bestLen);
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    string s;
    getline(cin, s);
    for (int i = 0; i < t; i++) {
        getline(cin, s);
        cout << longestPalindrome(s);
        if (i + 1 < t) cout << '\n';
    }
    return 0;
}
