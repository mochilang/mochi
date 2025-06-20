public class Main {
	static int[] reverseKGroup(int[] nums, int k) {
		int n = nums.length;
		if ((k <= 1)) {
			return nums;
		}
		Object[] result = new Object[]{};
		int i = 0;
		while ((i < n)) {
			int end = (i + k);
			if ((end <= n)) {
				int j = (end - 1);
				while ((j >= i)) {
					result = _concat(result, new int[]{nums[j]});
					j = (j - 1);
				}
			} else {
				int j = i;
				while ((j < n)) {
					result = _concat(result, new int[]{nums[j]});
					j = (j + 1);
				}
			}
			i = (i + k);
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
