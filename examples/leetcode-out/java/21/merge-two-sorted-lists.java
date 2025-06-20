public class Main {
	static int[] mergeTwoLists(int[] l1, int[] l2) {
		int i = 0;
		int j = 0;
		Object[] result = new Object[]{};
		while (((i < l1.length) && (j < l2.length))) {
			if ((l1[i] <= l2[j])) {
				result = _concat(result, new int[]{l1[i]});
				i = (i + 1);
			} else {
				result = _concat(result, new int[]{l2[j]});
				j = (j + 1);
			}
		}
		while ((i < l1.length)) {
			result = _concat(result, new int[]{l1[i]});
			i = (i + 1);
		}
		while ((j < l2.length)) {
			result = _concat(result, new int[]{l2[j]});
			j = (j + 1);
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
