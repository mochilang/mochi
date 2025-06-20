dynamic generateParenthesis(int n) {
	dynamic result = [];
	dynamic backtrack(String current, int open, int close) {
		if ((current.length == (n * 2))) {
			result = (result + [current]);
		} else {
			if ((open < n)) {
				backtrack((current + "("), (open + 1), close);
			}
			if ((close < open)) {
				backtrack((current + ")"), open, (close + 1));
			}
		}
	}
	backtrack("", 0, 0);
	return result;
}

void main() {
}

