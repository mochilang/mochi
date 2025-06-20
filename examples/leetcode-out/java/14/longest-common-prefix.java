public class Main {
	static String longestCommonPrefix(String[] strs) {
		if ((strs.length == 0)) {
			return "";
		}
		String prefix = strs[0];
		for (int i = 1; i < strs.length; i++) {
			int j = 0;
			String current = strs[i];
			while (((j < prefix.length()) && (j < current.length()))) {
				if ((_indexString(prefix, j) != _indexString(current, j))) {
					break;
				}
				j = (j + 1);
			}
			prefix = prefix.substring(0, j);
			if ((prefix == "")) {
				break;
			}
		}
		return prefix;
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
