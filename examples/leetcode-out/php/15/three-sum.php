<?php
function mochi_threeSum($nums) {
	$sorted = (function($tmp){ sort($tmp); return $tmp; })($nums);
	$n = (is_array($sorted) ? count($sorted) : strlen($sorted));
	$res = [];
	$i = 0;
	while (($i < $n)) {
		if ((($i > 0) && ($sorted[$i] == $sorted[($i - 1)]))) {
			$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
			continue;
		}
		$left = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
		$right = ($n - 1);
		while (($left < $right)) {
			$sum = ((is_array(((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left])))) && is_array($sorted[$right])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left]))), $sorted[$right]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left])))) || is_string($sorted[$right])) ? (((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left]))) . $sorted[$right]) : (((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left]))) + $sorted[$right])));
			if (($sum == 0)) {
				$res = ((is_array($res) && is_array([[$sorted[$i], $sorted[$left], $sorted[$right]]])) ? array_merge($res, [[$sorted[$i], $sorted[$left], $sorted[$right]]]) : ((is_string($res) || is_string([[$sorted[$i], $sorted[$left], $sorted[$right]]])) ? ($res . [[$sorted[$i], $sorted[$left], $sorted[$right]]]) : ($res + [[$sorted[$i], $sorted[$left], $sorted[$right]]])));
				$left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
				while ((($left < $right) && ($sorted[$left] == $sorted[($left - 1)]))) {
					$left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
				}
				$right = ($right - 1);
				while ((($left < $right) && ($sorted[$right] == $sorted[((is_array($right) && is_array(1)) ? array_merge($right, 1) : ((is_string($right) || is_string(1)) ? ($right . 1) : ($right + 1)))]))) {
					$right = ($right - 1);
				}
			} else 
			if (($sum < 0)) {
				$left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
			} else {
				$right = ($right - 1);
			}
		}
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	return $res;
}

function mochi_test_example_1() {
	if (!((mochi_threeSum([-1, 0, 1, 2, -1, -4]) == [[-1, -1, 2], [-1, 0, 1]]))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_threeSum([0, 1, 1]) == []))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_threeSum([0, 0, 0]) == [[0, 0, 0]]))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();
