#include <iostream>
#include <string>
#include <vector>

using namespace std;

static string quote(const string& s) {
    return "\"" + s + "\"";
}

int main() {
    int tc;
    if (!(cin >> tc)) return 0;
    vector<string> out;
    for (int t = 0; t < tc; t++) {
        string data;
        int q;
        cin >> data >> q;
        int pos = 0;
        string block = to_string(q);
        for (int i = 0; i < q; i++) {
            int n;
            cin >> n;
            string part = data.substr(pos, n);
            pos += (int)part.size();
            block += "\n" + quote(part);
        }
        out.push_back(block);
    }
    for (int i = 0; i < (int)out.size(); i++) {
        if (i) cout << "\n\n";
        cout << out[i];
    }
    return 0;
}
