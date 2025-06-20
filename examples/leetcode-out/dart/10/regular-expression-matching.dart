bool isMatch(String s, String p) {
	dynamic m = s.length;
	dynamic n = p.length;
	dynamic dp = [];
	dynamic i = 0;
	while ((i <= m)) {
		dynamic row = [];
		dynamic j = 0;
		while ((j <= n)) {
			row = (row + [false]);
			j = ((j + 1)).toInt();
		}
		dp = (dp + [row]);
		i = ((i + 1)).toInt();
	}
	dp[m][n] = true;
	dynamic i2 = m;
	while ((i2 >= 0)) {
		dynamic j2 = (n - 1);
		while ((j2 >= 0)) {
			dynamic first = false;
			if ((i2 < m)) {
				if ((((_indexString(p, j2) == _indexString(s, i2))) || ((_indexString(p, j2) == ".")))) {
					first = true;
				}
			}
			dynamic star = false;
			if (((j2 + 1) < n)) {
				if ((_indexString(p, ((j2 + 1)).toInt()) == "*")) {
					star = true;
				}
			}
			if (star) {
				dynamic ok = false;
				if (dp[i2][(j2 + 2)]) {
					ok = true;
				} else {
					if (first) {
						if (dp[(i2 + 1)][j2]) {
							ok = true;
						}
					}
				}
				dp[i2][j2] = ok;
			} else {
				dynamic ok = false;
				if (first) {
					if (dp[(i2 + 1)][(j2 + 1)]) {
						ok = true;
					}
				}
				dp[i2][j2] = ok;
			}
			j2 = ((j2 - 1)).toInt();
		}
		i2 = ((i2 - 1)).toInt();
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

