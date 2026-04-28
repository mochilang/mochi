#include <algorithm>
#include <iostream>
#include <set>
#include <utility>
#include <vector>

using Point = std::pair<int, int>;

static int cross(const Point& o, const Point& a, const Point& b) {
    return (a.first - o.first) * (b.second - o.second) - (a.second - o.second) * (b.first - o.first);
}

static std::vector<Point> solve(std::vector<Point> pts) {
    std::sort(pts.begin(), pts.end());
    pts.erase(std::unique(pts.begin(), pts.end()), pts.end());
    if (pts.size() <= 1) return pts;

    std::vector<Point> lower;
    for (const auto& p : pts) {
        while (lower.size() >= 2 && cross(lower[lower.size() - 2], lower.back(), p) < 0) {
            lower.pop_back();
        }
        lower.push_back(p);
    }

    std::vector<Point> upper;
    for (auto it = pts.rbegin(); it != pts.rend(); ++it) {
        while (upper.size() >= 2 && cross(upper[upper.size() - 2], upper.back(), *it) < 0) {
            upper.pop_back();
        }
        upper.push_back(*it);
    }

    std::set<Point> seen(lower.begin(), lower.end());
    seen.insert(upper.begin(), upper.end());
    return std::vector<Point>(seen.begin(), seen.end());
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);
    int t;
    if (!(std::cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n;
        std::cin >> n;
        std::vector<Point> pts(n);
        for (int i = 0; i < n; ++i) std::cin >> pts[i].first >> pts[i].second;
        auto hull = solve(pts);
        if (tc) std::cout << "\n";
        std::cout << hull.size() << "\n";
        for (const auto& [x, y] : hull) std::cout << x << " " << y << "\n";
    }
    return 0;
}
