dynamic isMatch(s, p) {
	dynamic m = s.length;
	dynamic n = p.length;
	dynamic memo = {};
	dynamic dfs(i, j) {
		dynamic key = ((i * ((n + 1))) + j);
		if ((memo.contains(key))) {
			return memo[key];
		}
		if ((j == n)) {
			return (i == m);
		}
		dynamic first = false;
		if ((i < m)) {
			if ((((p[j] == s[i])) || ((p[j] == ".")))) {
				first = true;
			}
		}
		dynamic ans = false;
		if (((j + 1) < n)) {
			if ((p[(j + 1)] == "*")) {
				if (dfs(i, (j + 2))) {
					ans = true;
				} else 
				if ((first && dfs((i + 1), j))) {
					ans = true;
				}
			} else {
				if ((first && dfs((i + 1), (j + 1)))) {
					ans = true;
				}
			}
		} else {
			if ((first && dfs((i + 1), (j + 1)))) {
				ans = true;
			}
		}
		memo[key] = ans;
		return ans;
	}
	return dfs(0, 0);
}

void main() {
}
