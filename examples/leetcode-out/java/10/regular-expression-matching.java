class Main {
	static boolean isMatch(String s, String p) {
		var m = s.length();
		var n = p.length();
		boolean[][] dp = new boolean[][]{};
		var i = 0;
		while ((i <= m)) {
			boolean[] row = new boolean[]{};
			var j = 0;
			while ((j <= n)) {
				row = _concat(row, new boolean[]{false});
				j = (j + 1);
			}
			dp = _concat(dp, new Object[]{row});
			i = (i + 1);
		}
		dp[m][n] = true;
		var i2 = m;
		while ((i2 >= 0)) {
			var j2 = (n - 1);
			while ((j2 >= 0)) {
				var first = false;
				if ((i2 < m)) {
					if (((_indexString(p, j2) == _indexString(s, i2)) || (_indexString(p, j2) == "."))) {
						first = true;
					}
				}
				if ((((j2 + 1) < n) && (_indexString(p, (j2 + 1)) == "*"))) {
					if ((dp[i2][(j2 + 2)] || (first && dp[(i2 + 1)][j2]))) {
						dp[i2][j2] = true;
					} else {
						dp[i2][j2] = false;
					}
				} else {
					if ((first && dp[(i2 + 1)][(j2 + 1)])) {
						dp[i2][j2] = true;
					} else {
						dp[i2][j2] = false;
					}
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
