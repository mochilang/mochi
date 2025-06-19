bool isPalindrome(int x) {
	if ((x < 0)) {
		return false;
	}
	String s = x.toString();
	int n = s.length;
	for (var i = 0; i < (n ~/ 2); i++) {
		if ((s[i] != s[((n - 1) - i)])) {
			return false;
		}
	}
	return true;
}

void main() {
}
