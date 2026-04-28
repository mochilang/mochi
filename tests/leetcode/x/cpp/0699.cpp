#include <iostream>
#include <string>
#include <vector>
using namespace std;

vector<int> solve(const vector<pair<int, int>>& positions) {
    vector<int> heights(positions.size());
    vector<int> answer;
    int best = 0;
    for (int i = 0; i < (int)positions.size(); i++) {
        int left = positions[i].first;
        int size = positions[i].second;
        int right = left + size;
        int base = 0;
        for (int j = 0; j < i; j++) {
            int otherLeft = positions[j].first;
            int otherRight = otherLeft + positions[j].second;
            if (left < otherRight && otherLeft < right) {
                base = max(base, heights[j]);
            }
        }
        heights[i] = base + size;
        best = max(best, heights[i]);
        answer.push_back(best);
    }
    return answer;
}

string fmt(const vector<int>& values) {
    string out = "[";
    for (int i = 0; i < (int)values.size(); i++) {
        if (i) out += ",";
        out += to_string(values[i]);
    }
    out += "]";
    return out;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n;
        cin >> n;
        vector<pair<int, int>> positions(n);
        for (int i = 0; i < n; i++) cin >> positions[i].first >> positions[i].second;
        if (tc) cout << "\n\n";
        cout << fmt(solve(positions));
    }
}
