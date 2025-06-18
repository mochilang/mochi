class Main {
	static int expand(String s, int left, int right) {
		var l = left;
		var r = right;
		var n = s.length();
		while (((l >= 0) && (r < n))) {
			if ((_indexString(s, l) != _indexString(s, r))) {
				break;
			}
			l = (l - 1);
			r = (r + 1);
		}
		return ((r - l) - 1);
	}
	
	static String longestPalindrome(String s) {
		if ((s.length() <= 1)) {
			return s;
		}
		var start = 0;
		var end = 0;
		var n = s.length();
		for (int i = 0; i < n; i++) {
			var len1 = expand(s, i, i);
			var len2 = expand(s, i, (i + 1));
			var l = len1;
			if ((len2 > len1)) {
				l = len2;
			}
			if ((l > (end - start))) {
				start = (i - ((l - 1) / 2));
				end = (i + (l / 2));
			}
		}
		return s.substring(start, (end + 1));
	}
	
	public static void main(String[] args) {
	}
	
	static String _indexString(String s, int i) {
		char[] runes = s.toCharArray();
		if (i < 0) i += runes.length;
		if (i < 0 || i >= runes.length) throw new RuntimeException("index out of range");
		return String.valueOf(runes[i]);
	}
}
