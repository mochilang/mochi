public class Main {
	static boolean isValid(String s) {
		Object[] stack = new Object[]{};
		int n = s.length();
		for (int i = 0; i < n; i++) {
			String c = _indexString(s, i);
			if ((c == "(")) {
				stack = _concat(stack, new String[]{")"});
			} else 			if ((c == "[")) {
				stack = _concat(stack, new String[]{"]"});
			} else 			if ((c == "{")) {
				stack = _concat(stack, new String[]{"}"});
			} else {
				if ((stack.length == 0)) {
					return false;
				}
				Object top = stack[(stack.length - 1)];
				if ((top != c)) {
					return false;
				}
				stack = _slice(stack, 0, (stack.length - 1));
			}
		}
		return (stack.length == 0);
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
	
	static int[] _slice(int[] arr, int i, int j) {
		if (i < 0) i += arr.length;
		if (j < 0) j += arr.length;
		if (i < 0) i = 0;
		if (j > arr.length) j = arr.length;
		if (j < i) j = i;
		int[] res = new int[j - i];
		System.arraycopy(arr, i, res, 0, j - i);
		return res;
	}
	
	static String _indexString(String s, int i) {
		char[] runes = s.toCharArray();
		if (i < 0) i += runes.length;
		if (i < 0 || i >= runes.length) throw new RuntimeException("index out of range");
		return String.valueOf(runes[i]);
	}
}
