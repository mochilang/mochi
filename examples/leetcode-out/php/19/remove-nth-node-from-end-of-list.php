<?php
function mochi_removeNthFromEnd($nums, $n) {
	$idx = ((is_array($nums) ? count($nums) : strlen($nums)) - $n);
	$result = [];
	$i = 0;
	while (($i < (is_array($nums) ? count($nums) : strlen($nums)))) {
		if (($i != $idx)) {
			$result = ((is_array($result) && is_array([$nums[$i]])) ? array_merge($result, [$nums[$i]]) : ((is_string($result) || is_string([$nums[$i]])) ? ($result . [$nums[$i]]) : ($result + [$nums[$i]])));
		}
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	return $result;
}

function mochi_test_example_1() {
	if (!((mochi_removeNthFromEnd([1, 2, 3, 4, 5], 2) == [1, 2, 3, 5]))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_removeNthFromEnd([1], 1) == []))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_removeNthFromEnd([1, 2], 1) == [1]))) { throw new Exception('expect failed'); }
}

function mochi_test_remove_first() {
	if (!((mochi_removeNthFromEnd([7, 8, 9], 3) == [8, 9]))) { throw new Exception('expect failed'); }
}

function mochi_test_remove_last() {
	if (!((mochi_removeNthFromEnd([7, 8, 9], 1) == [7, 8]))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();
mochi_test_remove_first();
mochi_test_remove_last();
