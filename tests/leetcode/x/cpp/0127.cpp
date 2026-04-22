#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_set>
#include <vector>

using namespace std;

static int ladderLength(const string& begin, const string& end, const vector<string>& words) {
    unordered_set<string> wordSet(words.begin(), words.end());
    if (!wordSet.count(end)) return 0;
    unordered_set<string> level{begin}, visited{begin};
    int steps = 1;
    while (!level.empty()) {
        if (level.count(end)) return steps;
        unordered_set<string> next;
        vector<string> cur(level.begin(), level.end());
        sort(cur.begin(), cur.end());
        for (const auto& word : cur) {
            string s = word;
            for (int i = 0; i < (int)s.size(); i++) {
                char orig = s[i];
                for (char c = 'a'; c <= 'z'; c++) {
                    if (c == orig) continue;
                    s[i] = c;
                    if (wordSet.count(s) && !visited.count(s)) next.insert(s);
                }
                s[i] = orig;
            }
        }
        for (const auto& word : next) visited.insert(word);
        level = next;
        steps++;
    }
    return 0;
}

int main() {
    int tc;
    if (!(cin >> tc)) return 0;
    vector<string> out;
    for (int t = 0; t < tc; t++) {
        string begin, end;
        int n;
        cin >> begin >> end >> n;
        vector<string> words(n);
        for (int i = 0; i < n; i++) cin >> words[i];
        out.push_back(to_string(ladderLength(begin, end, words)));
    }
    for (int i = 0; i < (int)out.size(); i++) {
        if (i) cout << "\n\n";
        cout << out[i];
    }
    return 0;
}
