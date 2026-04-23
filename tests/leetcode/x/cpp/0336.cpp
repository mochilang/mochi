#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
using namespace std;

bool pal(const string& s) { for (int i = 0, j = (int)s.size() - 1; i < j; ++i, --j) if (s[i] != s[j]) return false; return true; }
string rev(string s) { reverse(s.begin(), s.end()); return s; }
vector<pair<int,int>> palindromePairs(vector<string>& words) {
    unordered_map<string,int> pos; for (int i = 0; i < (int)words.size(); ++i) pos[words[i]] = i;
    vector<pair<int,int>> ans;
    for (int i = 0; i < (int)words.size(); ++i) { string& w = words[i]; for (int cut = 0; cut <= (int)w.size(); ++cut) { string pre = w.substr(0, cut), suf = w.substr(cut); if (pal(pre)) { auto it = pos.find(rev(suf)); if (it != pos.end() && it->second != i) ans.push_back({it->second, i}); } if (cut < (int)w.size() && pal(suf)) { auto it = pos.find(rev(pre)); if (it != pos.end() && it->second != i) ans.push_back({i, it->second}); } } }
    sort(ans.begin(), ans.end()); return ans;
}
string fmt(const vector<pair<int,int>>& p) { string s = "["; for (int i = 0; i < (int)p.size(); ++i) { if (i) s += ","; s += "[" + to_string(p[i].first) + "," + to_string(p[i].second) + "]"; } return s + "]"; }
int main() { ios::sync_with_stdio(false); cin.tie(nullptr); int t; if (!(cin >> t)) return 0; for (int tc = 0; tc < t; ++tc) { int n; cin >> n; vector<string> words(n); for (auto &w : words) { cin >> w; if (w == "_") w = ""; } if (tc) cout << "\n\n"; cout << fmt(palindromePairs(words)); } }
