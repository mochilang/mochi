#include <iostream>
#include <string>
using namespace std;

static string convertZigzag(const string& s, int numRows) {
    if (numRows <= 1 || numRows >= (int)s.size()) return s;
    int cycle = 2 * numRows - 2;
    string out;
    out.reserve(s.size());
    for (int row = 0; row < numRows; row++) {
        for (int i = row; i < (int)s.size(); i += cycle) {
            out.push_back(s[i]);
            int diag = i + cycle - 2 * row;
            if (row > 0 && row < numRows - 1 && diag < (int)s.size()) out.push_back(s[diag]);
        }
    }
    return out;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    string s, line;
    getline(cin, line);
    for (int caseNo = 0; caseNo < t; caseNo++) {
        getline(cin, s);
        getline(cin, line);
        int numRows = stoi(line);
        cout << convertZigzag(s, numRows);
        if (caseNo + 1 < t) cout << '\n';
    }
    return 0;
}
