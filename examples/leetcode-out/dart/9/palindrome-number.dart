dynamic isPalindrome(x) {
	if ((x < 0)) {
		return false;
	}
	dynamic s = x.toString();
	dynamic n = s.length;
	for (var i = 0; i < (n ~/ 2); i++) {
		if ((s[i] != s[((n - 1) - i)])) {
			return false;
		}
	}
	return true;
}

void main() {
}
