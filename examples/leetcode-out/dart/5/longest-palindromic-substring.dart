dynamic expand(s, left, right) {
	dynamic l = left;
	dynamic r = right;
	dynamic n = s.length;
	while (((l >= 0) && (r < n))) {
		if ((s[l] != s[r])) {
			break;
		}
		l = (l - 1);
		r = (r + 1);
	}
	return ((r - l) - 1);
}

dynamic longestPalindrome(s) {
	if ((s.length <= 1)) {
		return s;
	}
	dynamic start = 0;
	dynamic end = 0;
	dynamic n = s.length;
	for (var i = 0; i < n; i++) {
		dynamic len1 = expand(s, i, i);
		dynamic len2 = expand(s, i, (i + 1));
		dynamic l = len1;
		if ((len2 > len1)) {
			l = len2;
		}
		if ((l > ((end - start)))) {
			start = (i - ((((l - 1)) ~/ 2)));
			end = (i + ((l ~/ 2)));
		}
	}
	dynamic res = "";
	dynamic k = start;
	while ((k <= end)) {
		res = (res + s[k]);
		k = (k + 1);
	}
	return res;
}

void main() {
}
