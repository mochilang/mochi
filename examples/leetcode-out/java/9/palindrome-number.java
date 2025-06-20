public class Main {
	static boolean isPalindrome(int x) {
		if ((x < 0)) {
			return false;
		}
		String s = String.valueOf(x);
		int n = s.length();
		for (int i = 0; i < (n / 2); i++) {
			if ((_indexString(s, i) != _indexString(s, ((n - 1) - i)))) {
				return false;
			}
		}
		return true;
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
