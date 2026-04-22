#include <algorithm>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

struct MedianFinder {
    vector<int> data;

    void addNum(int num) {
        auto it = lower_bound(data.begin(), data.end(), num);
        data.insert(it, num);
    }

    double findMedian() const {
        int n = static_cast<int>(data.size());
        if (n % 2 == 1) {
            return data[n / 2];
        }
        return (data[n / 2 - 1] + data[n / 2]) / 2.0;
    }
};

int main() {
    int t;
    if (!(cin >> t)) return 0;
    cout << fixed << setprecision(1);
    for (int tc = 0; tc < t; ++tc) {
        int m;
        cin >> m;
        MedianFinder mf;
        if (tc) cout << "\n\n";
        bool first = true;
        for (int i = 0; i < m; ++i) {
            string op;
            cin >> op;
            if (op == "addNum") {
                int x;
                cin >> x;
                mf.addNum(x);
            } else {
                if (!first) cout << '\n';
                first = false;
                cout << mf.findMedian();
            }
        }
    }
    return 0;
}
