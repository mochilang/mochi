#include <algorithm>
#include <functional>
#include <iostream>
#include <string>
#include <unordered_set>
#include <vector>

using namespace std;

static vector<string> wordBreak(const string& s, const vector<string>& words) {
    unordered_set<string> wordSet(words.begin(), words.end());
    vector<int> lengths;
    for (const auto& w : words) lengths.push_back((int)w.size());
    sort(lengths.begin(), lengths.end());
    lengths.erase(unique(lengths.begin(), lengths.end()), lengths.end());
    vector<int> seen(s.size() + 1, 0);
    vector<vector<string>> memo(s.size() + 1);
    function<vector<string>(int)> dfs = [&](int i) -> vector<string> {
        if (seen[i]) return memo[i];
        seen[i] = 1;
        vector<string> out;
        if (i == (int)s.size()) {
            out.push_back("");
        } else {
            for (int length : lengths) {
                int j = i + length;
                if (j > (int)s.size()) break;
                string word = s.substr(i, length);
                if (wordSet.count(word)) {
                    for (const auto& tail : dfs(j)) {
                        out.push_back(tail.empty() ? word : word + " " + tail);
                    }
                }
            }
            sort(out.begin(), out.end());
        }
        memo[i] = out;
        return out;
    };
    return dfs(0);
}

int main() {
    int tc;
    if (!(cin >> tc)) return 0;
    vector<vector<string>> blocks;
    for (int t = 0; t < tc; t++) {
        string s;
        int n;
        cin >> s >> n;
        vector<string> words(n);
        for (int i = 0; i < n; i++) cin >> words[i];
        auto ans = wordBreak(s, words);
        vector<string> block;
        block.push_back(to_string(ans.size()));
        for (const auto& sentence : ans) block.push_back(sentence);
        blocks.push_back(block);
    }
    for (int i = 0; i < (int)blocks.size(); i++) {
        if (i) cout << "\n\n";
        for (int j = 0; j < (int)blocks[i].size(); j++) {
            if (j) cout << "\n";
            cout << blocks[i][j];
        }
    }
    return 0;
}
