#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

static int candy(const vector<int>& ratings) {
    int n = static_cast<int>(ratings.size());
    vector<int> candies(n, 1);
    for (int i = 1; i < n; i++) {
        if (ratings[i] > ratings[i - 1]) candies[i] = candies[i - 1] + 1;
    }
    for (int i = n - 2; i >= 0; i--) {
        if (ratings[i] > ratings[i + 1]) candies[i] = max(candies[i], candies[i + 1] + 1);
    }
    int total = 0;
    for (int v : candies) total += v;
    return total;
}

int main() {
    int tc;
    if (!(cin >> tc)) return 0;
    vector<string> out;
    for (int t = 0; t < tc; t++) {
        int n;
        cin >> n;
        vector<int> ratings(n);
        for (int i = 0; i < n; i++) cin >> ratings[i];
        out.push_back(to_string(candy(ratings)));
    }
    for (int i = 0; i < (int)out.size(); i++) {
        if (i) cout << "\n\n";
        cout << out[i];
    }
    return 0;
}
