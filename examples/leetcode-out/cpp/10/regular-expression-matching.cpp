#include <bits/stdc++.h>
using namespace std;

string _indexString(const string& s, int i) {
	int n = s.size();
	if (i < 0) i += n;
	if (i < 0 || i >= n) throw std::out_of_range("index out of range");
	return string(1, s[i]);
}

bool isMatch(string s, string p){
	auto m = s.size();
	auto n = p.size();
	vector<vector<bool>> dp = vector<vector<bool>>{};
	int i = 0;
	while (i <= m) {
		vector<bool> row = vector<bool>{};
		int j = 0;
		while (j <= n) {
			row = ([&](vector<bool> a, vector<bool> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(row, vector<bool>{false});
			j = j + 1;
		}
		dp = ([&](vector<vector<bool>> a, vector<vector<bool>> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(dp, vector<vector<bool>>{row});
		i = i + 1;
	}
	dp[m][n] = true;
	auto i2 = m;
	while (i2 >= 0) {
		auto j2 = n - 1;
		while (j2 >= 0) {
			bool first = false;
			if (i2 < m) {
				if ((_indexString(p, j2) == _indexString(s, i2)) || (_indexString(p, j2) == string("."))) {
					first = true;
				}
			}
			bool star = false;
			if (j2 + 1 < n) {
				if (_indexString(p, j2 + 1) == string("*")) {
					star = true;
				}
			}
			if (star) {
				if (dp[i2][j2 + 2] || (first && dp[i2 + 1][j2])) {
					dp[i2][j2] = true;
				} else {
					dp[i2][j2] = false;
				}
			} else {
				if (first && dp[i2 + 1][j2 + 1]) {
					dp[i2][j2] = true;
				} else {
					dp[i2][j2] = false;
				}
			}
			j2 = j2 - 1;
		}
		i2 = i2 - 1;
	}
	return dp[0][0];
}

int main() {
	return 0;
}
