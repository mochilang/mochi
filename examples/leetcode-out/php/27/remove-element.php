<?php
function mochi_removeElement(&$nums, $val) {
	$k = 0;
	$i = 0;
	while (($i < (is_array($nums) ? count($nums) : strlen($nums)))) {
		if (($nums[$i] != $val)) {
			$nums[$k] = $nums[$i];
			$k = ((is_array($k) && is_array(1)) ? array_merge($k, 1) : ((is_string($k) || is_string(1)) ? ($k . 1) : ($k + 1)));
		}
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	return $k;
}

function mochi_test_example_1() {
	$nums = [3, 2, 2, 3];
	$k = mochi_removeElement($nums, 3);
	if (!(($k == 2))) { throw new Exception('expect failed'); }
	if (!(((is_array($nums) ? array_slice($nums, 0, ($k) - (0)) : substr($nums, 0, ($k) - (0))) == [2, 2]))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	$nums = [0, 1, 2, 2, 3, 0, 4, 2];
	$k = mochi_removeElement($nums, 2);
	if (!(($k == 5))) { throw new Exception('expect failed'); }
	if (!(((is_array($nums) ? array_slice($nums, 0, ($k) - (0)) : substr($nums, 0, ($k) - (0))) == [0, 1, 3, 0, 4]))) { throw new Exception('expect failed'); }
}

function mochi_test_no_removal() {
	$nums = [1, 2, 3];
	$k = mochi_removeElement($nums, 4);
	if (!(($k == 3))) { throw new Exception('expect failed'); }
	if (!(((is_array($nums) ? array_slice($nums, 0, ($k) - (0)) : substr($nums, 0, ($k) - (0))) == [1, 2, 3]))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_no_removal();
