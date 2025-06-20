public class Main {
	static String convert(String s, int numRows) {
		if (((numRows <= 1) || (numRows >= s.length()))) {
			return s;
		}
		String[] rows = new String[]{};
		int i = 0;
		while ((i < numRows)) {
			rows = _concat(rows, new String[]{""});
			i = (i + 1);
		}
		int curr = 0;
		int step = 1;
		for (var ch : s.toCharArray()) {
			rows[curr] = (rows[curr] + ch);
			if ((curr == 0)) {
				step = 1;
			} else 			if ((curr == (numRows - 1))) {
				step = (-1);
			}
			curr = (curr + step);
		}
		String result = "";
		for (var row : rows) {
			result = (result + row);
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
