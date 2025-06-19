class Main {
	static boolean isPalindrome(int x) {
		if ((x < 0)) {
			return false;
		}
		var s = String.valueOf(x);
		var n = s.length;
		for (int i = 0; i < (n / 2); i++) {
			if ((s[i] != s[((n - 1) - i)])) {
				return false;
			}
		}
		return true;
	}
	
	public static void main(String[] args) {
	}
}
