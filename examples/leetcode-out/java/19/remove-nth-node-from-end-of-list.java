public class Main {
	static int[] removeNthFromEnd(int[] nums, int n) {
		int idx = (nums.length - n);
		Object[] result = new Object[]{};
		int i = 0;
		while ((i < nums.length)) {
			if ((i != idx)) {
				result = _concat(result, new int[]{nums[i]});
			}
			i = (i + 1);
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
