public class Main {
	static boolean isMatch(String s, String p) {
		int m = s.length();
		int n = p.length();
		boolean[][] dp = new boolean[][]{};
		int i = 0;
		while ((i <= m)) {
			boolean[] row = new boolean[]{};
			int j = 0;
			while ((j <= n)) {
				row = _concat(row, new boolean[]{false});
				j = (j + 1);
			}
			dp = _concat(dp, new boolean[][]{row});
			i = (i + 1);
		}
		dp[m][n] = true;
		int i2 = m;
		while ((i2 >= 0)) {
			int j2 = (n - 1);
			while ((j2 >= 0)) {
				boolean first = false;
				if ((i2 < m)) {
					if (((_indexString(p, j2) == _indexString(s, i2)) || (_indexString(p, j2) == "."))) {
						first = true;
					}
				}
				boolean star = false;
				if (((j2 + 1) < n)) {
					if ((_indexString(p, (j2 + 1)) == "*")) {
						star = true;
					}
				}
				if (star) {
					boolean ok = false;
					if (dp[i2][(j2 + 2)]) {
						ok = true;
					} else {
						if (first) {
							if (dp[(i2 + 1)][j2]) {
								ok = true;
							}
						}
					}
					dp[i2][j2] = ok;
				} else {
					boolean ok = false;
					if (first) {
						if (dp[(i2 + 1)][(j2 + 1)]) {
							ok = true;
						}
					}
					dp[i2][j2] = ok;
				}
				j2 = (j2 - 1);
			}
			i2 = (i2 - 1);
		}
		return dp[0][0];
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
	
	static String _indexString(String s, int i) {
		char[] runes = s.toCharArray();
		if (i < 0) i += runes.length;
		if (i < 0 || i >= runes.length) throw new RuntimeException("index out of range");
		return String.valueOf(runes[i]);
	}
}
