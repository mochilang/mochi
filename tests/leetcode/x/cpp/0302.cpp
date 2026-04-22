#include <iostream>
#include <string>
#include <vector>

using namespace std;

int minArea(const vector<string>& image, int x, int y) {
    int top = image.size(), bottom = -1;
    int left = image[0].size(), right = -1;
    for (int i = 0; i < (int)image.size(); ++i) {
        for (int j = 0; j < (int)image[i].size(); ++j) {
            if (image[i][j] == '1') {
                if (i < top) top = i;
                if (i > bottom) bottom = i;
                if (j < left) left = j;
                if (j > right) right = j;
            }
        }
    }
    return (bottom - top + 1) * (right - left + 1);
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int r, c;
        cin >> r >> c;
        vector<string> image(r);
        for (int i = 0; i < r; ++i) cin >> image[i];
        int x, y;
        cin >> x >> y;
        if (tc) cout << "\n\n";
        cout << minArea(image, x, y);
    }
    return 0;
}
