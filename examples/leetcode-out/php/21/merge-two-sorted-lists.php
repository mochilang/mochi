<?php
function mochi_mergeTwoLists($l1, $l2) {
	$i = 0;
	$j = 0;
	$result = [];
	while ((($i < (is_array($l1) ? count($l1) : strlen($l1))) && ($j < (is_array($l2) ? count($l2) : strlen($l2))))) {
		if (($l1[$i] <= $l2[$j])) {
			$result = ((is_array($result) && is_array([$l1[$i]])) ? array_merge($result, [$l1[$i]]) : ((is_string($result) || is_string([$l1[$i]])) ? ($result . [$l1[$i]]) : ($result + [$l1[$i]])));
			$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
		} else {
			$result = ((is_array($result) && is_array([$l2[$j]])) ? array_merge($result, [$l2[$j]]) : ((is_string($result) || is_string([$l2[$j]])) ? ($result . [$l2[$j]]) : ($result + [$l2[$j]])));
			$j = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
		}
	}
	while (($i < (is_array($l1) ? count($l1) : strlen($l1)))) {
		$result = ((is_array($result) && is_array([$l1[$i]])) ? array_merge($result, [$l1[$i]]) : ((is_string($result) || is_string([$l1[$i]])) ? ($result . [$l1[$i]]) : ($result + [$l1[$i]])));
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	while (($j < (is_array($l2) ? count($l2) : strlen($l2)))) {
		$result = ((is_array($result) && is_array([$l2[$j]])) ? array_merge($result, [$l2[$j]]) : ((is_string($result) || is_string([$l2[$j]])) ? ($result . [$l2[$j]]) : ($result + [$l2[$j]])));
		$j = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
	}
	return $result;
}

function mochi_test_example_1() {
	if (!((mochi_mergeTwoLists([1, 2, 4], [1, 3, 4]) == [1, 1, 2, 3, 4, 4]))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_mergeTwoLists([], []) == []))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_mergeTwoLists([], [0]) == [0]))) { throw new Exception('expect failed'); }
}

function mochi_test_different_lengths() {
	if (!((mochi_mergeTwoLists([1, 5, 7], [2, 3, 4, 6, 8]) == [1, 2, 3, 4, 5, 6, 7, 8]))) { throw new Exception('expect failed'); }
}

function mochi_test_one_list_empty() {
	if (!((mochi_mergeTwoLists([1, 2, 3], []) == [1, 2, 3]))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();
mochi_test_different_lengths();
mochi_test_one_list_empty();
