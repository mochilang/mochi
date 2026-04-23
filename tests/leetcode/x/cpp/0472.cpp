#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_set>
#include <vector>
using namespace std;

bool can_form(const string& word, const unordered_set<string>& seen) {
    if (seen.empty()) return false;
    vector<char> dp(word.size() + 1, 0);
    dp[0] = 1;
    for (size_t i = 1; i <= word.size(); ++i) {
        for (size_t j = 0; j < i; ++j) {
            if (dp[j] && seen.count(word.substr(j, i - j))) {
                dp[i] = 1;
                break;
            }
        }
    }
    return dp[word.size()];
}

string fmt(vector<string> words) {
    sort(words.begin(), words.end());
    string out = "[";
    for (size_t i = 0; i < words.size(); ++i) {
        if (i) out += ",";
        out += "\"" + words[i] + "\"";
    }
    out += "]";
    return out;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n;
        cin >> n;
        vector<string> words(n);
        for (int i = 0; i < n; ++i) cin >> words[i];
        vector<string> ordered = words;
        sort(ordered.begin(), ordered.end(), [](const string& a, const string& b) {
            if (a.size() != b.size()) return a.size() < b.size();
            return a < b;
        });
        unordered_set<string> seen;
        vector<string> ans;
        for (const string& word : ordered) {
            if (can_form(word, seen)) ans.push_back(word);
            seen.insert(word);
        }
        if (tc) cout << "\n\n";
        cout << fmt(ans);
    }
    return 0;
}
