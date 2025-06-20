<?php
function mochi_removeDuplicates($nums) {
	if (((is_array($nums) ? count($nums) : strlen($nums)) == 0)) {
		return 0;
	}
	$count = 1;
	$prev = $nums[0];
	$i = 1;
	while (($i < (is_array($nums) ? count($nums) : strlen($nums)))) {
		$cur = $nums[$i];
		if (($cur != $prev)) {
			$count = ((is_array($count) && is_array(1)) ? array_merge($count, 1) : ((is_string($count) || is_string(1)) ? ($count . 1) : ($count + 1)));
			$prev = $cur;
		}
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	return $count;
}

function mochi_test_example_1() {
	if (!((mochi_removeDuplicates([1, 1, 2]) == 2))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_removeDuplicates([0, 0, 1, 1, 1, 2, 2, 3, 3, 4]) == 5))) { throw new Exception('expect failed'); }
}

function mochi_test_empty() {
	if (!((mochi_removeDuplicates([]) == 0))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_empty();
