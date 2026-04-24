#include <algorithm>
#include <iostream>
#include <queue>
#include <utility>
#include <vector>

static int solve(int k, int w, const std::vector<int>& profits, const std::vector<int>& capital) {
    std::vector<std::pair<int, int>> projects;
    projects.reserve(profits.size());
    for (std::size_t i = 0; i < profits.size(); ++i) {
        projects.push_back({capital[i], profits[i]});
    }
    std::sort(projects.begin(), projects.end());
    std::priority_queue<int> pq;
    int cur = w;
    std::size_t idx = 0;
    for (int step = 0; step < k; ++step) {
        while (idx < projects.size() && projects[idx].first <= cur) {
            pq.push(projects[idx].second);
            ++idx;
        }
        if (pq.empty()) {
            break;
        }
        cur += pq.top();
        pq.pop();
    }
    return cur;
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int t;
    if (!(std::cin >> t)) {
        return 0;
    }
    for (int tc = 0; tc < t; ++tc) {
        int n, k, w;
        std::cin >> n >> k >> w;
        std::vector<int> profits(n), capital(n);
        for (int i = 0; i < n; ++i) {
            std::cin >> profits[i];
        }
        for (int i = 0; i < n; ++i) {
            std::cin >> capital[i];
        }
        if (tc) {
            std::cout << "\n\n";
        }
        std::cout << solve(k, w, profits, capital);
    }
    return 0;
}
