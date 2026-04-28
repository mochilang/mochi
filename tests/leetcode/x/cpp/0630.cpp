#include <algorithm>
#include <iostream>
#include <queue>
#include <vector>

struct Course {
    int duration;
    int last_day;
};

static int solve(std::vector<Course> courses) {
    std::sort(courses.begin(), courses.end(), [](const Course& a, const Course& b) {
        return a.last_day < b.last_day;
    });
    int total = 0;
    std::priority_queue<int> pq;
    for (const auto& c : courses) {
        total += c.duration;
        pq.push(c.duration);
        if (total > c.last_day) {
            total -= pq.top();
            pq.pop();
        }
    }
    return static_cast<int>(pq.size());
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);
    int t;
    if (!(std::cin >> t)) return 0;
    for (int i = 0; i < t; ++i) {
        int n;
        std::cin >> n;
        std::vector<Course> courses(n);
        for (int j = 0; j < n; ++j) {
            std::cin >> courses[j].duration >> courses[j].last_day;
        }
        if (i) std::cout << "\n\n";
        std::cout << solve(courses);
    }
    return 0;
}
