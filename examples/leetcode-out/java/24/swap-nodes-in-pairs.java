public class Main {
	static int[] swapPairs(int[] nums) {
		int i = 0;
		Object[] result = new Object[]{};
		while ((i < nums.length)) {
			if (((i + 1) < nums.length)) {
				result = _concat(result, new int[]{nums[(i + 1)], nums[i]});
			} else {
				result = _concat(result, new int[]{nums[i]});
			}
			i = (i + 2);
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
