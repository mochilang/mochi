#include <iostream>
#include <vector>
#include <string>
using namespace std;

string getPermutation(int n, int k) {
  vector<string> digits;
  for (int i = 1; i <= n; i++) digits.push_back(to_string(i));
  vector<int> fact(n + 1, 1);
  for (int i = 1; i <= n; i++) fact[i] = fact[i - 1] * i;
  k--;
  string out;
  for (int rem = n; rem >= 1; rem--) {
    int block = fact[rem - 1];
    int idx = k / block;
    k %= block;
    out += digits[idx];
    digits.erase(digits.begin() + idx);
  }
  return out;
}

int main() {
  ios::sync_with_stdio(false);
  cin.tie(nullptr);
  int t;
  if (!(cin >> t)) return 0;
  for (int tc = 0; tc < t; tc++) {
    int n, k;
    cin >> n >> k;
    cout << getPermutation(n, k);
    if (tc + 1 < t) cout << '\n';
  }
  return 0;
}
