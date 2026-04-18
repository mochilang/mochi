#include <iostream>
#include <string>
#include <vector>
using namespace std;

static bool isValid(const string& s) {
    vector<char> st;
    for (char ch : s) {
        if (ch == '(' || ch == '[' || ch == '{') {
            st.push_back(ch);
        } else {
            if (st.empty()) return false;
            char open = st.back();
            st.pop_back();
            if ((ch == ')' && open != '(') ||
                (ch == ']' && open != '[') ||
                (ch == '}' && open != '{')) return false;
        }
    }
    return st.empty();
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    string s;
    while (t--) {
        cin >> s;
        cout << (isValid(s) ? "true" : "false") << "\n";
    }
    return 0;
}
