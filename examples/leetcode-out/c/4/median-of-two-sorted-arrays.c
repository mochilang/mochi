#include <stdio.h>
#include <stdlib.h>

typedef struct { int len; int *data; } list_int;

static list_int list_int_create(int len) {
	list_int l;
	l.len = len;
	l.data = (int*)malloc(sizeof(int)*len);
	return l;
}

static list_int concat_list_int(list_int a, list_int b) {
	list_int r = list_int_create(a.len + b.len);
	for (int i = 0; i < a.len; i++) {
		r.data[i] = a.data[i];
	}
	for (int i = 0; i < b.len; i++) {
		r.data[a.len + i] = b.data[i];
	}
	return r;
}


double findMedianSortedArrays(list_int nums1, list_int nums2){
	list_int _t1 = list_int_create(0);
	list_int merged = _t1;
	int i = 0;
	int j = 0;
	while ((((i < nums1.len) || j) < nums2.len)) {
		if ((j >= nums2.len)) {
			list_int _t2 = list_int_create(1);
			_t2.data[0] = nums1.data[i];
			list_int _t3 = concat_list_int(merged, _t2);
			merged = _t3;
			i = (i + 1);
		} else 		if ((i >= nums1.len)) {
			list_int _t4 = list_int_create(1);
			_t4.data[0] = nums2.data[j];
			list_int _t5 = concat_list_int(merged, _t4);
			merged = _t5;
			j = (j + 1);
		} else 		if ((nums1.data[i] <= nums2.data[j])) {
			list_int _t6 = list_int_create(1);
			_t6.data[0] = nums1.data[i];
			list_int _t7 = concat_list_int(merged, _t6);
			merged = _t7;
			i = (i + 1);
		} else {
			list_int _t8 = list_int_create(1);
			_t8.data[0] = nums2.data[j];
			list_int _t9 = concat_list_int(merged, _t8);
			merged = _t9;
			j = (j + 1);
		}
	}
	int total = merged.len;
	if (((total % 2) == 1)) {
		return merged.data[(total / 2)];
	}
	int mid1 = merged.data[((total / 2) - 1)];
	int mid2 = merged.data[(total / 2)];
	return (((mid1 + mid2)) / 2);
}

int main() {
	return 0;
}
