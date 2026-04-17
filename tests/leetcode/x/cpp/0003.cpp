#include <iostream>
#include <string>
#include <unordered_map>
using namespace std;

static int longest(const string& s) {
    unordered_map<char, int> last;
    int left = 0, best = 0;
    for (int right = 0; right < (int)s.size(); right++) {
        char ch = s[right];
        if (last.count(ch) && last[ch] >= left) left = last[ch] + 1;
        last[ch] = right;
        if (right - left + 1 > best) best = right - left + 1;
    }
    return best;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t; if (!(cin >> t)) return 0;
    string s; getline(cin, s);
    for (int i = 0; i < t; i++) {
        getline(cin, s);
        cout << longest(s);
        if (i + 1 < t) cout << "\n";
    }
    return 0;
}
