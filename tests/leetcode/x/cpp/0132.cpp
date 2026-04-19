#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

static int minCut(const string& s) {
    int n = static_cast<int>(s.size());
    vector<vector<bool>> pal(n, vector<bool>(n, false));
    vector<int> cuts(n, 0);
    for (int end = 0; end < n; end++) {
        cuts[end] = end;
        for (int start = 0; start <= end; start++) {
            if (s[start] == s[end] && (end - start <= 2 || pal[start + 1][end - 1])) {
                pal[start][end] = true;
                if (start == 0) cuts[end] = 0;
                else cuts[end] = min(cuts[end], cuts[start - 1] + 1);
            }
        }
    }
    return cuts[n - 1];
}

int main() {
    int tc;
    if (!(cin >> tc)) return 0;
    vector<string> out;
    for (int i = 0; i < tc; i++) {
        string s;
        cin >> s;
        out.push_back(to_string(minCut(s)));
    }
    for (int i = 0; i < (int)out.size(); i++) {
        if (i) cout << "\n\n";
        cout << out[i];
    }
    return 0;
}
