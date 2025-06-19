<?php
function findMedianSortedArrays($nums1, $nums2) {
	$merged = [];
	$i = 0;
	$j = 0;
	while ((($i < (is_array($nums1) ? count($nums1) : strlen($nums1))) || ($j < (is_array($nums2) ? count($nums2) : strlen($nums2))))) {
		if (($j >= (is_array($nums2) ? count($nums2) : strlen($nums2)))) {
			$merged = ((is_array($merged) && is_array([$nums1[$i]])) ? array_merge($merged, [$nums1[$i]]) : ((is_string($merged) || is_string([$nums1[$i]])) ? ($merged . [$nums1[$i]]) : ($merged + [$nums1[$i]])));
			$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
		} else 
		if (($i >= (is_array($nums1) ? count($nums1) : strlen($nums1)))) {
			$merged = ((is_array($merged) && is_array([$nums2[$j]])) ? array_merge($merged, [$nums2[$j]]) : ((is_string($merged) || is_string([$nums2[$j]])) ? ($merged . [$nums2[$j]]) : ($merged + [$nums2[$j]])));
			$j = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
		} else 
		if (($nums1[$i] <= $nums2[$j])) {
			$merged = ((is_array($merged) && is_array([$nums1[$i]])) ? array_merge($merged, [$nums1[$i]]) : ((is_string($merged) || is_string([$nums1[$i]])) ? ($merged . [$nums1[$i]]) : ($merged + [$nums1[$i]])));
			$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
		} else {
			$merged = ((is_array($merged) && is_array([$nums2[$j]])) ? array_merge($merged, [$nums2[$j]]) : ((is_string($merged) || is_string([$nums2[$j]])) ? ($merged . [$nums2[$j]]) : ($merged + [$nums2[$j]])));
			$j = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
		}
	}
	$total = (is_array($merged) ? count($merged) : strlen($merged));
	if ((((is_int($total) && is_int(2)) ? ($total % 2) : fmod($total, 2)) == 1)) {
		return $merged[((is_int($total) && is_int(2)) ? intdiv($total, 2) : ($total / 2))];
	}
	$mid1 = $merged[(((is_int($total) && is_int(2)) ? intdiv($total, 2) : ($total / 2)) - 1)];
	$mid2 = $merged[((is_int($total) && is_int(2)) ? intdiv($total, 2) : ($total / 2))];
	return ((is_int((((is_array($mid1) && is_array($mid2)) ? array_merge($mid1, $mid2) : ((is_string($mid1) || is_string($mid2)) ? ($mid1 . $mid2) : ($mid1 + $mid2))))) && is_int(2.0)) ? intdiv((((is_array($mid1) && is_array($mid2)) ? array_merge($mid1, $mid2) : ((is_string($mid1) || is_string($mid2)) ? ($mid1 . $mid2) : ($mid1 + $mid2)))), 2.0) : ((((is_array($mid1) && is_array($mid2)) ? array_merge($mid1, $mid2) : ((is_string($mid1) || is_string($mid2)) ? ($mid1 . $mid2) : ($mid1 + $mid2)))) / 2.0));
}

function test_example_1() {
	if (!((findMedianSortedArrays([1, 3], [2]) == 2.0))) { throw new Exception('expect failed'); }
}

function test_example_2() {
	if (!((findMedianSortedArrays([1, 2], [3, 4]) == 2.5))) { throw new Exception('expect failed'); }
}

function test_empty_first() {
	if (!((findMedianSortedArrays([], [1]) == 1.0))) { throw new Exception('expect failed'); }
}

function test_empty_second() {
	if (!((findMedianSortedArrays([2], []) == 2.0))) { throw new Exception('expect failed'); }
}

test_example_1();
test_example_2();
test_empty_first();
test_empty_second();
