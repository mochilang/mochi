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
	auto i = 0;
	while (i <= m) {
		vector<bool> row = vector<bool>{};
		auto j = 0;
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
			auto first = false;
			if (i2 < m) {
				if ((_indexString(p, j2) == _indexString(s, i2)) || (_indexString(p, j2) == string("."))) {
					first = true;
				}
			}
			auto star = false;
			if (j2 + 1 < n) {
				if (_indexString(p, j2 + 1) == string("*")) {
					star = true;
				}
			}
			if (star) {
				auto ok = false;
				if (dp[i2][j2 + 2]) {
					ok = true;
				} else {
					if (first) {
						if (dp[i2 + 1][j2]) {
							ok = true;
						}
					}
				}
				dp[i2][j2] = ok;
			} else {
				auto ok = false;
				if (first) {
					if (dp[i2 + 1][j2 + 1]) {
						ok = true;
					}
				}
				dp[i2][j2] = ok;
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
