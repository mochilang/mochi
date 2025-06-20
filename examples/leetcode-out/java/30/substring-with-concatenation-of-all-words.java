public class Main {
	static int[] findSubstring(String s, String[] words) {
		if ((words.length == 0)) {
			return new int[]{};
		}
		int wordLen = words[0].length;
		int wordCount = words.length;
		int totalLen = (wordLen * wordCount);
		if ((s.length() < totalLen)) {
			return new int[]{};
		}
		java.util.Map<String, Integer> freq = new java.util.HashMap<>();
		for (var w : words) {
			if (freq.containsKey(w)) {
				freq.put(w, (freq.get(w) + 1));
			} else {
				freq.put(w, 1);
			}
		}
		int[] result = new int[]{};
		for (int offset = 0; offset < wordLen; offset++) {
			int left = offset;
			int count = 0;
			java.util.Map<String, Integer> seen = new java.util.HashMap<>();
			int j = offset;
			while (((j + wordLen) <= s.length())) {
				String word = s.substring(j, (j + wordLen));
				j = (j + wordLen);
				if (freq.containsKey(word)) {
					if (seen.containsKey(word)) {
						seen.put(word, (seen.get(word) + 1));
					} else {
						seen.put(word, 1);
					}
					count = (count + 1);
					while ((seen.get(word) > freq.get(word))) {
						String lw = s.substring(left, (left + wordLen));
						seen.put(lw, (seen.get(lw) - 1));
						left = (left + wordLen);
						count = (count - 1);
					}
					if ((count == wordCount)) {
						result = _concat(result, new int[]{left});
						String lw = s.substring(left, (left + wordLen));
						seen.put(lw, (seen.get(lw) - 1));
						left = (left + wordLen);
						count = (count - 1);
					}
				} else {
					seen = new java.util.HashMap<>();
					count = 0;
					left = j;
				}
			}
		}
		return result;
	}
	
	public static void main(String[] args) {
	}
	
	static int[] _concat(int[] a, int[] b) {
		int[] res = new int[a.length + b.length];
		System.arraycopy(a, 0, res, 0, a.length);
		System.arraycopy(b, 0, res, a.length, b.length);
		return res;
	}
	
	static boolean[] _concat(boolean[] a, boolean[] b) {
		boolean[] res = new boolean[a.length + b.length];
		System.arraycopy(a, 0, res, 0, a.length);
		System.arraycopy(b, 0, res, a.length, b.length);
		return res;
	}
	
	static <T> T[] _concat(T[] a, T[] b) {
		T[] res = java.util.Arrays.copyOf(a, a.length + b.length);
		System.arraycopy(b, 0, res, a.length, b.length);
		return res;
	}
}
