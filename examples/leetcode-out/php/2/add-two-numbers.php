<?php
function addTwoNumbers($l1, $l2) {
	$i = 0;
	$j = 0;
	$carry = 0;
	$result = [];
	while (((($i < (is_array($l1) ? count($l1) : strlen($l1))) || ($j < (is_array($l2) ? count($l2) : strlen($l2)))) || ($carry > 0))) {
		$x = 0;
		if (($i < (is_array($l1) ? count($l1) : strlen($l1)))) {
			$x = $l1[$i];
			$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
		}
		$y = 0;
		if (($j < (is_array($l2) ? count($l2) : strlen($l2)))) {
			$y = $l2[$j];
			$j = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
		}
		$sum = ((is_array(((is_array($x) && is_array($y)) ? array_merge($x, $y) : ((is_string($x) || is_string($y)) ? ($x . $y) : ($x + $y)))) && is_array($carry)) ? array_merge(((is_array($x) && is_array($y)) ? array_merge($x, $y) : ((is_string($x) || is_string($y)) ? ($x . $y) : ($x + $y))), $carry) : ((is_string(((is_array($x) && is_array($y)) ? array_merge($x, $y) : ((is_string($x) || is_string($y)) ? ($x . $y) : ($x + $y)))) || is_string($carry)) ? (((is_array($x) && is_array($y)) ? array_merge($x, $y) : ((is_string($x) || is_string($y)) ? ($x . $y) : ($x + $y))) . $carry) : (((is_array($x) && is_array($y)) ? array_merge($x, $y) : ((is_string($x) || is_string($y)) ? ($x . $y) : ($x + $y))) + $carry)));
		$digit = ((is_int($sum) && is_int(10)) ? ($sum % 10) : fmod($sum, 10));
		$carry = ((is_int($sum) && is_int(10)) ? intdiv($sum, 10) : ($sum / 10));
		$result = ((is_array($result) && is_array([$digit])) ? array_merge($result, [$digit]) : ((is_string($result) || is_string([$digit])) ? ($result . [$digit]) : ($result + [$digit])));
	}
	return $result;
}

function test_example_1() {
	if (!((addTwoNumbers([2, 4, 3], [5, 6, 4]) == [7, 0, 8]))) { throw new Exception('expect failed'); }
}

function test_example_2() {
	if (!((addTwoNumbers([0], [0]) == [0]))) { throw new Exception('expect failed'); }
}

function test_example_3() {
	if (!((addTwoNumbers([9, 9, 9, 9, 9, 9, 9], [9, 9, 9, 9]) == [8, 9, 9, 9, 0, 0, 0, 1]))) { throw new Exception('expect failed'); }
}

test_example_1();
test_example_2();
test_example_3();
