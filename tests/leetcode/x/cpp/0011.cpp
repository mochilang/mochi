#include <iostream>
#include <vector>
using namespace std;

static int maxArea(const vector<int>& h) {
    int left = 0, right = (int)h.size() - 1, best = 0;
    while (left < right) {
        int height = min(h[left], h[right]);
        best = max(best, (right - left) * height);
        if (h[left] < h[right]) left++; else right--;
    }
    return best;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; tc++) {
        int n; cin >> n;
        vector<int> h(n);
        for (int i = 0; i < n; i++) cin >> h[i];
        cout << maxArea(h);
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
