#include <array>
#include <functional>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

int solve(const vector<string>& stickers, const string& target) {
    vector<array<int, 26>> stickerCounts(stickers.size());
    for (int i = 0; i < (int)stickers.size(); i++) {
        for (char ch : stickers[i]) {
            stickerCounts[i][ch - 'a']++;
        }
    }
    vector<int> targetIdx(target.size());
    for (int i = 0; i < (int)target.size(); i++) {
        targetIdx[i] = target[i] - 'a';
    }
    int fullMask = (1 << target.size()) - 1;
    vector<int> memo(1 << target.size(), -2);
    function<int(int)> dfs = [&](int mask) -> int {
        if (mask == fullMask) return 0;
        int& cached = memo[mask];
        if (cached != -2) return cached;
        int first = 0;
        while ((mask >> first) & 1) first++;
        int need = targetIdx[first];
        int best = 1000000000;
        for (const auto& counts : stickerCounts) {
            if (counts[need] == 0) continue;
            auto remaining = counts;
            int nextMask = mask;
            for (int i = 0; i < (int)targetIdx.size(); i++) {
                int ch = targetIdx[i];
                if (((nextMask >> i) & 1) == 0 && remaining[ch] > 0) {
                    remaining[ch]--;
                    nextMask |= 1 << i;
                }
            }
            if (nextMask != mask) {
                best = min(best, 1 + dfs(nextMask));
            }
        }
        cached = best;
        return cached;
    };
    int answer = dfs(0);
    return answer >= 1000000000 ? -1 : answer;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n;
        cin >> n;
        vector<string> stickers(n);
        for (int i = 0; i < n; i++) cin >> stickers[i];
        string target;
        cin >> target;
        if (tc) cout << "\n\n";
        cout << solve(stickers, target);
    }
}
