<?php
function findMedianSortedArrays($nums1, $nums2) {
	$merged = [];
	$i = 0;
	$j = 0;
	while (((($i < count($nums1)) || $j) < count($nums2))) {
		if (($j >= count($nums2))) {
			$merged = ((is_array($merged) && is_array([$nums1[$i]])) ? array_merge($merged, [$nums1[$i]]) : ((is_string($merged) || is_string([$nums1[$i]])) ? ($merged . [$nums1[$i]]) : ($merged + [$nums1[$i]])));
			$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
		} else 
		if (($i >= count($nums1))) {
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
	$total = count($merged);
	if ((($total % 2) == 1)) {
		return $merged[($total / 2)];
	}
	$mid1 = $merged[(($total / 2) - 1)];
	$mid2 = $merged[($total / 2)];
	return ((((is_array($mid1) && is_array($mid2)) ? array_merge($mid1, $mid2) : ((is_string($mid1) || is_string($mid2)) ? ($mid1 . $mid2) : ($mid1 + $mid2)))) / 2);
}

