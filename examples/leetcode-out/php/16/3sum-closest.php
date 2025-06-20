<?php
function mochi_threeSumClosest($nums, $target) {
	$sorted = (function($tmp){ sort($tmp); return $tmp; })($nums);
	$n = (is_array($sorted) ? count($sorted) : strlen($sorted));
	$best = ((is_array(((is_array($sorted[0]) && is_array($sorted[1])) ? array_merge($sorted[0], $sorted[1]) : ((is_string($sorted[0]) || is_string($sorted[1])) ? ($sorted[0] . $sorted[1]) : ($sorted[0] + $sorted[1])))) && is_array($sorted[2])) ? array_merge(((is_array($sorted[0]) && is_array($sorted[1])) ? array_merge($sorted[0], $sorted[1]) : ((is_string($sorted[0]) || is_string($sorted[1])) ? ($sorted[0] . $sorted[1]) : ($sorted[0] + $sorted[1]))), $sorted[2]) : ((is_string(((is_array($sorted[0]) && is_array($sorted[1])) ? array_merge($sorted[0], $sorted[1]) : ((is_string($sorted[0]) || is_string($sorted[1])) ? ($sorted[0] . $sorted[1]) : ($sorted[0] + $sorted[1])))) || is_string($sorted[2])) ? (((is_array($sorted[0]) && is_array($sorted[1])) ? array_merge($sorted[0], $sorted[1]) : ((is_string($sorted[0]) || is_string($sorted[1])) ? ($sorted[0] . $sorted[1]) : ($sorted[0] + $sorted[1]))) . $sorted[2]) : (((is_array($sorted[0]) && is_array($sorted[1])) ? array_merge($sorted[0], $sorted[1]) : ((is_string($sorted[0]) || is_string($sorted[1])) ? ($sorted[0] . $sorted[1]) : ($sorted[0] + $sorted[1]))) + $sorted[2])));
	for ($i = 0; $i < $n; $i++) {
		$left = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
		$right = ($n - 1);
		while (($left < $right)) {
			$sum = ((is_array(((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left])))) && is_array($sorted[$right])) ? array_merge(((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left]))), $sorted[$right]) : ((is_string(((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left])))) || is_string($sorted[$right])) ? (((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left]))) . $sorted[$right]) : (((is_array($sorted[$i]) && is_array($sorted[$left])) ? array_merge($sorted[$i], $sorted[$left]) : ((is_string($sorted[$i]) || is_string($sorted[$left])) ? ($sorted[$i] . $sorted[$left]) : ($sorted[$i] + $sorted[$left]))) + $sorted[$right])));
			if (($sum == $target)) {
				return $target;
			}
			$diff = 0;
			if (($sum > $target)) {
				$diff = ($sum - $target);
			} else {
				$diff = ($target - $sum);
			}
			$bestDiff = 0;
			if (($best > $target)) {
				$bestDiff = ($best - $target);
			} else {
				$bestDiff = ($target - $best);
			}
			if (($diff < $bestDiff)) {
				$best = $sum;
			}
			if (($sum < $target)) {
				$left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
			} else {
				$right = ($right - 1);
			}
		}
	}
	return $best;
}

function mochi_test_example_1() {
	if (!((mochi_threeSumClosest([-1, 2, 1, -4], 1) == 2))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_threeSumClosest([0, 0, 0], 1) == 0))) { throw new Exception('expect failed'); }
}

function mochi_test_additional() {
	if (!((mochi_threeSumClosest([1, 1, 1, 0], -100) == 2))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_additional();
