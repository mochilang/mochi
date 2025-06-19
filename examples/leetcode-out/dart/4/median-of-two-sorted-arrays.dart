double findMedianSortedArrays(List<int> nums1, List<int> nums2) {
	List<int> merged = [];
	int i = 0;
	int j = 0;
	while (((i < nums1.length) || (j < nums2.length))) {
		if ((j >= nums2.length)) {
			merged = (merged + [nums1[i]]);
			i = (i + 1);
		} else 
		if ((i >= nums1.length)) {
			merged = (merged + [nums2[j]]);
			j = (j + 1);
		} else 
		if ((nums1[i] <= nums2[j])) {
			merged = (merged + [nums1[i]]);
			i = (i + 1);
		} else {
			merged = (merged + [nums2[j]]);
			j = (j + 1);
		}
	}
	int total = merged.length;
	if (((total % 2) == 1)) {
		return (merged[(total ~/ 2)]).toDouble();
	}
	var mid1 = merged[((total ~/ 2) - 1)];
	var mid2 = merged[(total ~/ 2)];
	return ((((mid1 + mid2))).toDouble() / 2);
}

void main() {
}
