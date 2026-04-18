#include <iostream>
#include <vector>
using namespace std;
int main() {
    int t; if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n, k; cin >> n; vector<int> arr(n);
        for (int i = 0; i < n; ++i) cin >> arr[i];
        cin >> k;
        for (int i = 0; i + k <= n; i += k) for (int l = i, r = i + k - 1; l < r; ++l, --r) swap(arr[l], arr[r]);
        cout << '['; for (int i = 0; i < n; ++i) { if (i) cout << ','; cout << arr[i]; } cout << ']';
        if (tc + 1 < t) cout << '\n';
    }
    return 0;
}
