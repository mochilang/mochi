<?php
function mochi_fourSum($nums, $target) {
	$sorted = (function($tmp){ sort($tmp); return $tmp; })($nums);
	$n = (is_array($sorted) ? count($sorted) : strlen($sorted));
	$result = [];
	for ($i = 0; $i < $n; $i++) {
		if ((($i > 0) && ($sorted[$i] == $sorted[($i - 1)]))) {
			continue;
		}
		for ($j = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1))); $j < $n; $j++) {
			if ((($j > ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)))) && ($sorted[$j] == $sorted[($j - 1)]))) {
				continue;
			}
			$left = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
			$right = ($n - 1);
			while (($left < $right)) {
				$sum = ((is_array(((is_array(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) && is_array($sorted[$left])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))), $sorted[$left]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) || is_string($sorted[$left])) ? (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) . $sorted[$left]) : (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) + $sorted[$left])))) && is_array($sorted[$right])) ? array_merge(((is_array(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) && is_array($sorted[$left])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))), $sorted[$left]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) || is_string($sorted[$left])) ? (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) . $sorted[$left]) : (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) + $sorted[$left]))), $sorted[$right]) : ((is_string(((is_array(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) && is_array($sorted[$left])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))), $sorted[$left]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) || is_string($sorted[$left])) ? (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) . $sorted[$left]) : (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) + $sorted[$left])))) || is_string($sorted[$right])) ? (((is_array(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) && is_array($sorted[$left])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))), $sorted[$left]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) || is_string($sorted[$left])) ? (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) . $sorted[$left]) : (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) + $sorted[$left]))) . $sorted[$right]) : (((is_array(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) && is_array($sorted[$left])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))), $sorted[$left]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j])))) || is_string($sorted[$left])) ? (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) . $sorted[$left]) : (((is_array($sorted[$i]) && is_array($sorted[$j])) ? array_merge($sorted[$i], $sorted[$j]) : ((is_string($sorted[$i]) || is_string($sorted[$j])) ? ($sorted[$i] . $sorted[$j]) : ($sorted[$i] + $sorted[$j]))) + $sorted[$left]))) + $sorted[$right])));
				if (($sum == $target)) {
					$result = ((is_array($result) && is_array([[$sorted[$i], $sorted[$j], $sorted[$left], $sorted[$right]]])) ? array_merge($result, [[$sorted[$i], $sorted[$j], $sorted[$left], $sorted[$right]]]) : ((is_string($result) || is_string([[$sorted[$i], $sorted[$j], $sorted[$left], $sorted[$right]]])) ? ($result . [[$sorted[$i], $sorted[$j], $sorted[$left], $sorted[$right]]]) : ($result + [[$sorted[$i], $sorted[$j], $sorted[$left], $sorted[$right]]])));
					$left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
					$right = ($right - 1);
					while ((($left < $right) && ($sorted[$left] == $sorted[($left - 1)]))) {
						$left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
					}
					while ((($left < $right) && ($sorted[$right] == $sorted[((is_array($right) && is_array(1)) ? array_merge($right, 1) : ((is_string($right) || is_string(1)) ? ($right . 1) : ($right + 1)))]))) {
						$right = ($right - 1);
					}
				} else 
				if (($sum < $target)) {
					$left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
				} else {
					$right = ($right - 1);
				}
			}
		}
	}
	return $result;
}

function mochi_test_example_1() {
	if (!((mochi_fourSum([1, 0, -1, 0, -2, 2], 0) == [[-2, -1, 1, 2], [-2, 0, 0, 2], [-1, 0, 0, 1]]))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_fourSum([2, 2, 2, 2, 2], 8) == [[2, 2, 2, 2]]))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
