class Main {
	static double findMedianSortedArrays(int[] nums1, int[] nums2) {
		int[] merged = new int[]{};
		var i = 0;
		var j = 0;
		while (((i < nums1.length) || (j < nums2.length))) {
			if ((j >= nums2.length)) {
				merged = _concat(merged, new int[]{nums1[i]});
				i = (i + 1);
			} else 			if ((i >= nums1.length)) {
				merged = _concat(merged, new int[]{nums2[j]});
				j = (j + 1);
			} else 			if ((nums1[i] <= nums2[j])) {
				merged = _concat(merged, new int[]{nums1[i]});
				i = (i + 1);
			} else {
				merged = _concat(merged, new int[]{nums2[j]});
				j = (j + 1);
			}
		}
		var total = merged.length;
		if (((total % 2) == 1)) {
			return (double)(merged[(total / 2)]);
		}
		var mid1 = merged[((total / 2) - 1)];
		var mid2 = merged[(total / 2)];
		return ((double)((mid1 + mid2)) / 2);
	}
	
	public static void main(String[] args) {
	}
	
	static int[] _concat(int[] a, int[] b) {
		int[] res = new int[a.length + b.length];
		System.arraycopy(a, 0, res, 0, a.length);
		System.arraycopy(b, 0, res, a.length, b.length);
		return res;
	}
}
