bool isMatch(String s, String p) {
	int m = s.length;
	int n = p.length;
	List<List<bool>> dp = [];
	int i = 0;
	while ((i <= m)) {
		List<bool> row = [];
		int j = 0;
		while ((j <= n)) {
			row = (row + [false]);
			j = (j + 1);
		}
		dp = (dp + [row]);
		i = (i + 1);
	}
	dp[m][n] = true;
	var i2 = m;
	while ((i2 >= 0)) {
		var j2 = (n - 1);
		while ((j2 >= 0)) {
			bool first = false;
			if ((i2 < m)) {
				if ((((_indexString(p, j2) == _indexString(s, i2))) || ((_indexString(p, j2) == ".")))) {
					first = true;
				}
			}
			if ((((j2 + 1) < n) && (_indexString(p, (j2 + 1)) == "*"))) {
				if ((dp[i2][(j2 + 2)] || ((first && dp[(i2 + 1)][j2])))) {
					dp[i2][j2] = true;
				} else {
					dp[i2][j2] = false;
				}
			} else {
				if ((first && dp[(i2 + 1)][(j2 + 1)])) {
					dp[i2][j2] = true;
				} else {
					dp[i2][j2] = false;
				}
			}
			j2 = (j2 - 1);
		}
		i2 = (i2 - 1);
	}
	return dp[0][0];
}

void main() {
}

String _indexString(String s, int i) {
	var runes = s.runes.toList();
	if (i < 0) {
		i += runes.length;
	}
	if (i < 0 || i >= runes.length) {
		throw RangeError('index out of range');
	}
	return String.fromCharCode(runes[i]);
}
