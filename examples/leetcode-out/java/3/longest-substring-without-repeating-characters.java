class Main {
	static int lengthOfLongestSubstring(String s) {
		var n = s.length();
		var start = 0;
		var best = 0;
		var i = 0;
		while ((i < n)) {
			var j = start;
			while ((j < i)) {
				if ((_indexString(s, j) == _indexString(s, i))) {
					start = (j + 1);
					break;
				}
				j = (j + 1);
			}
			var length = ((i - start) + 1);
			if ((length > best)) {
				best = length;
			}
			i = (i + 1);
		}
		return best;
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
