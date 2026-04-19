#include <algorithm>
#include <functional>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

using namespace std;

static vector<vector<string>> ladders(string begin, string end, vector<string> words) {
    unordered_set<string> wordSet(words.begin(), words.end());
    if (!wordSet.count(end)) return {};
    unordered_map<string, vector<string>> parents;
    unordered_set<string> level{begin}, visited{begin};
    bool found = false;
    while (!level.empty() && !found) {
        unordered_set<string> next;
        vector<string> cur(level.begin(), level.end());
        sort(cur.begin(), cur.end());
        for (auto &word : cur) {
            string s = word;
            for (int i = 0; i < (int)s.size(); i++) {
                char orig = s[i];
                for (char c = 'a'; c <= 'z'; c++) {
                    if (c == orig) continue;
                    s[i] = c;
                    if (!wordSet.count(s) || visited.count(s)) continue;
                    next.insert(s);
                    parents[s].push_back(word);
                    if (s == end) found = true;
                }
                s[i] = orig;
            }
        }
        for (auto &w : next) visited.insert(w);
        level = move(next);
    }
    if (!found) return {};
    vector<vector<string>> out; vector<string> path{end};
    function<void(const string&)> dfs = [&](const string& word) {
        if (word == begin) {
            vector<string> seq(path.rbegin(), path.rend());
            out.push_back(seq);
            return;
        }
        auto plist = parents[word];
        sort(plist.begin(), plist.end());
        for (auto &p : plist) {
            path.push_back(p);
            dfs(p);
            path.pop_back();
        }
    };
    dfs(end);
    sort(out.begin(), out.end());
    return out;
}

int main() {
    int tc;
    if (!(cin >> tc)) return 0;
    vector<string> blocks;
    for (int t = 0; t < tc; t++) {
        string begin, end; int n;
        cin >> begin >> end >> n;
        vector<string> words(n);
        for (int i = 0; i < n; i++) cin >> words[i];
        auto ans = ladders(begin, end, words);
        string block = to_string(ans.size());
        for (auto &p : ans) {
            block += "\n";
            for (int i = 0; i < (int)p.size(); i++) {
                if (i) block += "->";
                block += p[i];
            }
        }
        blocks.push_back(block);
    }
    for (int i = 0; i < (int)blocks.size(); i++) {
        if (i) cout << "\n\n";
        cout << blocks[i];
    }
    return 0;
}
