#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

struct Item {
    std::string word;
    int idx;
};

static int lcp(const std::string& a, const std::string& b) {
    int i = 0;
    while (i < static_cast<int>(a.size()) && i < static_cast<int>(b.size()) && a[i] == b[i]) {
        ++i;
    }
    return i;
}

static std::string abbreviate(const std::string& word, int prefix) {
    if (static_cast<int>(word.size()) - prefix <= 2) {
        return word;
    }
    return word.substr(0, prefix) + std::to_string(static_cast<int>(word.size()) - prefix - 1) + word.back();
}

static std::vector<std::string> solve(const std::vector<std::string>& words) {
    std::unordered_map<std::string, std::vector<Item>> groups;
    for (int i = 0; i < static_cast<int>(words.size()); ++i) {
        const std::string& word = words[i];
        std::string key = std::to_string(word.size()) + "|" + word.front() + "|" + word.back();
        groups[key].push_back({word, i});
    }
    std::vector<std::string> ans(words.size());
    for (auto& [_, group] : groups) {
        std::sort(group.begin(), group.end(), [](const Item& a, const Item& b) {
            return a.word < b.word;
        });
        for (int j = 0; j < static_cast<int>(group.size()); ++j) {
            int need = 1;
            if (j > 0) {
                need = std::max(need, lcp(group[j].word, group[j - 1].word) + 1);
            }
            if (j + 1 < static_cast<int>(group.size())) {
                need = std::max(need, lcp(group[j].word, group[j + 1].word) + 1);
            }
            ans[group[j].idx] = abbreviate(group[j].word, need);
        }
    }
    return ans;
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int t;
    if (!(std::cin >> t)) {
        return 0;
    }
    for (int tc = 0; tc < t; ++tc) {
        int n;
        std::cin >> n;
        std::vector<std::string> words(n);
        for (int i = 0; i < n; ++i) {
            std::cin >> words[i];
        }
        auto ans = solve(words);
        if (tc) {
            std::cout << "\n";
        }
        std::cout << ans.size() << "\n";
        for (const auto& s : ans) {
            std::cout << s << "\n";
        }
    }
    return 0;
}
