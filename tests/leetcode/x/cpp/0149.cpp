#include <algorithm>
#include <iostream>
#include <numeric>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

struct Point { int x, y; };

static int maxPoints(const vector<Point>& points) {
    int n = (int)points.size();
    if (n <= 2) return n;
    int best = 0;
    for (int i = 0; i < n; i++) {
        unordered_map<string, int> slopes;
        int local = 0;
        for (int j = i + 1; j < n; j++) {
            int dx = points[j].x - points[i].x;
            int dy = points[j].y - points[i].y;
            int g = gcd(dx, dy);
            dx /= g;
            dy /= g;
            if (dx < 0) {
                dx = -dx;
                dy = -dy;
            } else if (dx == 0) {
                dy = 1;
            } else if (dy == 0) {
                dx = 1;
            }
            string key = to_string(dy) + "/" + to_string(dx);
            local = max(local, ++slopes[key]);
        }
        best = max(best, local + 1);
    }
    return best;
}

int main() {
    int tc;
    if (!(cin >> tc)) return 0;
    vector<string> out;
    for (int t = 0; t < tc; t++) {
        int n;
        cin >> n;
        vector<Point> points(n);
        for (int i = 0; i < n; i++) cin >> points[i].x >> points[i].y;
        out.push_back(to_string(maxPoints(points)));
    }
    for (int i = 0; i < (int)out.size(); i++) {
        if (i) cout << "\n\n";
        cout << out[i];
    }
    return 0;
}
