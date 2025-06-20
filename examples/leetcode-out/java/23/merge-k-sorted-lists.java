public class Main {
	static int[] mergeKLists(int[][] lists) {
		int k = lists.length;
		int[] indices = new int[]{};
		int i = 0;
		while ((i < k)) {
			indices = _concat(indices, new int[]{0});
			i = (i + 1);
		}
		int[] result = new int[]{};
		while (true) {
			int best = 0;
			int bestList = (-1);
			boolean found = false;
			int j = 0;
			while ((j < k)) {
				int idx = indices[j];
				if ((idx < lists[j].length)) {
					int val = lists[j][idx];
					if (((!found) || (val < best))) {
						best = val;
						bestList = j;
						found = true;
					}
				}
				j = (j + 1);
			}
			if ((!found)) {
				break;
			}
			result = _concat(result, new int[]{best});
			indices[bestList] = (indices[bestList] + 1);
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
