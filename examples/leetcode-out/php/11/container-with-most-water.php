<?php
function mochi_maxArea($height) {
	$left = 0;
	$right = ((is_array($height) ? count($height) : strlen($height)) - 1);
	$maxArea = 0;
	while (($left < $right)) {
		$width = ($right - $left);
		$h = 0;
		if (($height[$left] < $height[$right])) {
			$h = $height[$left];
		} else {
			$h = $height[$right];
		}
		$area = ($h * $width);
		if (($area > $maxArea)) {
			$maxArea = $area;
		}
		if (($height[$left] < $height[$right])) {
			$left = ((is_array($left) && is_array(1)) ? array_merge($left, 1) : ((is_string($left) || is_string(1)) ? ($left . 1) : ($left + 1)));
		} else {
			$right = ($right - 1);
		}
	}
	return $maxArea;
}

function mochi_test_example_1() {
	if (!((mochi_maxArea([1, 8, 6, 2, 5, 4, 8, 3, 7]) == 49))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_maxArea([1, 1]) == 1))) { throw new Exception('expect failed'); }
}

function mochi_test_decreasing_heights() {
	if (!((mochi_maxArea([4, 3, 2, 1, 4]) == 16))) { throw new Exception('expect failed'); }
}

function mochi_test_short_array() {
	if (!((mochi_maxArea([1, 2, 1]) == 2))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_decreasing_heights();
mochi_test_short_array();
