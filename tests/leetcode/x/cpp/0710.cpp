#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
using namespace std;

struct Solution {
    int bound;
    unordered_map<int, int> remap;
    unsigned int state;

    Solution(int n, const vector<int>& blacklist) : state(1) {
        unordered_set<int> blocked(blacklist.begin(), blacklist.end());
        bound = n - (int)blacklist.size();
        vector<int> tail;
        for (int value = bound; value < n; value++) {
            if (!blocked.count(value)) tail.push_back(value);
        }
        int idx = 0;
        for (int value : blacklist) {
            if (value < bound) remap[value] = tail[idx++];
        }
    }

    int pick() {
        state = state * 1664525u + 1013904223u;
        int value = (int)(state % (unsigned int)bound);
        auto it = remap.find(value);
        return it == remap.end() ? value : it->second;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int t;
    if (!(cin >> t)) return 0;
    vector<string> cases;
    while (t--) {
        int ops;
        cin >> ops;
        vector<string> out;
        Solution* solution = nullptr;
        for (int i = 0; i < ops; i++) {
            string op;
            cin >> op;
            if (op == "C") {
                int n, b;
                cin >> n >> b;
                vector<int> blacklist(b);
                for (int j = 0; j < b; j++) cin >> blacklist[j];
                delete solution;
                solution = new Solution(n, blacklist);
                out.push_back("null");
            } else {
                out.push_back(to_string(solution->pick()));
            }
        }
        delete solution;
        string line = "[";
        for (int i = 0; i < (int)out.size(); i++) {
            if (i) line += ",";
            line += out[i];
        }
        line += "]";
        cases.push_back(line);
    }
    for (int i = 0; i < (int)cases.size(); i++) {
        if (i) cout << "\n\n";
        cout << cases[i];
    }
}
