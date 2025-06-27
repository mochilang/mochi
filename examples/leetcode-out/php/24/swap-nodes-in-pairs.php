<?php
function mochi_swapPairs($nums) {
	$i = 0;
	$result = [];
	while (($i < (is_array($nums) ? count($nums) : strlen($nums)))) {
		if ((((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1))) < (is_array($nums) ? count($nums) : strlen($nums)))) {
			$result = ((is_array($result) && is_array([$nums[((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)))], $nums[$i]])) ? array_merge($result, [$nums[((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)))], $nums[$i]]) : ((is_string($result) || is_string([$nums[((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)))], $nums[$i]])) ? ($result . [$nums[((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)))], $nums[$i]]) : ($result + [$nums[((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)))], $nums[$i]])));
		} else {
			$result = ((is_array($result) && is_array([$nums[$i]])) ? array_merge($result, [$nums[$i]]) : ((is_string($result) || is_string([$nums[$i]])) ? ($result . [$nums[$i]]) : ($result + [$nums[$i]])));
		}
		$i = ((is_array($i) && is_array(2)) ? array_merge($i, 2) : ((is_string($i) || is_string(2)) ? ($i . 2) : ($i + 2)));
	}
	return $result;
}

function mochi_test_example_1() {
	if (!((mochi_swapPairs([1, 2, 3, 4]) == [2, 1, 4, 3]))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_swapPairs([]) == []))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_swapPairs([1]) == [1]))) { throw new Exception('expect failed'); }
}

function mochi_test_odd_length() {
	if (!((mochi_swapPairs([1, 2, 3]) == [2, 1, 3]))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();
mochi_test_odd_length();
