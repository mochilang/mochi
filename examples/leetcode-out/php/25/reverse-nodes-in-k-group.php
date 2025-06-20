<?php
function mochi_reverseKGroup($nums, $k) {
	$n = (is_array($nums) ? count($nums) : strlen($nums));
	if (($k <= 1)) {
		return $nums;
	}
	$result = [];
	$i = 0;
	while (($i < $n)) {
		$end = ((is_array($i) && is_array($k)) ? array_merge($i, $k) : ((is_string($i) || is_string($k)) ? ($i . $k) : ($i + $k)));
		if (($end <= $n)) {
			$j = ($end - 1);
			while (($j >= $i)) {
				$result = ((is_array($result) && is_array([$nums[$j]])) ? array_merge($result, [$nums[$j]]) : ((is_string($result) || is_string([$nums[$j]])) ? ($result . [$nums[$j]]) : ($result + [$nums[$j]])));
				$j = ($j - 1);
			}
		} else {
			$j = $i;
			while (($j < $n)) {
				$result = ((is_array($result) && is_array([$nums[$j]])) ? array_merge($result, [$nums[$j]]) : ((is_string($result) || is_string([$nums[$j]])) ? ($result . [$nums[$j]]) : ($result + [$nums[$j]])));
				$j = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
			}
		}
		$i = ((is_array($i) && is_array($k)) ? array_merge($i, $k) : ((is_string($i) || is_string($k)) ? ($i . $k) : ($i + $k)));
	}
	return $result;
}

function mochi_test_example_1() {
	if (!((mochi_reverseKGroup([1, 2, 3, 4, 5], 2) == [2, 1, 4, 3, 5]))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_reverseKGroup([1, 2, 3, 4, 5], 3) == [3, 2, 1, 4, 5]))) { throw new Exception('expect failed'); }
}

function mochi_test_k_equals_list_length() {
	if (!((mochi_reverseKGroup([1, 2, 3, 4], 4) == [4, 3, 2, 1]))) { throw new Exception('expect failed'); }
}

function mochi_test_k_greater_than_length() {
	if (!((mochi_reverseKGroup([1, 2, 3], 5) == [1, 2, 3]))) { throw new Exception('expect failed'); }
}

function mochi_test_k_is_one() {
	if (!((mochi_reverseKGroup([1, 2, 3], 1) == [1, 2, 3]))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_k_equals_list_length();
mochi_test_k_greater_than_length();
mochi_test_k_is_one();
