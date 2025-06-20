double findMedianSortedArrays(nums1, nums2) {
	dynamic merged = [];
	dynamic i = 0;
	dynamic j = 0;
	while (((i < nums1.length) || (j < nums2.length))) {
		if ((j >= nums2.length)) {
			merged = (merged + [nums1[i]]);
			i = ((i + 1)).toInt();
		} else 
		if ((i >= nums1.length)) {
			merged = (merged + [nums2[j]]);
			j = ((j + 1)).toInt();
		} else 
		if ((nums1[i] <= nums2[j])) {
			merged = (merged + [nums1[i]]);
			i = ((i + 1)).toInt();
		} else {
			merged = (merged + [nums2[j]]);
			j = ((j + 1)).toInt();
		}
	}
	dynamic total = merged.length;
	if (((total % 2) == 1)) {
		return (merged[(total ~/ 2)]).toDouble();
	}
	dynamic mid1 = merged[((total ~/ 2) - 1)];
	dynamic mid2 = merged[(total ~/ 2)];
	return ((((mid1 + mid2))).toDouble() / 2);
}

void main() {
}

