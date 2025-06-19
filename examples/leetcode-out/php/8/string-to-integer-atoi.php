<?php
function digit($ch) {
	if (($ch == "0")) {
		return 0;
	}
	if (($ch == "1")) {
		return 1;
	}
	if (($ch == "2")) {
		return 2;
	}
	if (($ch == "3")) {
		return 3;
	}
	if (($ch == "4")) {
		return 4;
	}
	if (($ch == "5")) {
		return 5;
	}
	if (($ch == "6")) {
		return 6;
	}
	if (($ch == "7")) {
		return 7;
	}
	if (($ch == "8")) {
		return 8;
	}
	if (($ch == "9")) {
		return 9;
	}
	return -1;
}

function myAtoi($s) {
	$i = 0;
	$n = (is_array($s) ? count($s) : strlen($s));
	while ((($i < $n) && ($s[$i] == " "[0]))) {
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	$sign = 1;
	if ((($i < $n) && ((($s[$i] == "+"[0]) || ($s[$i] == "-"[0]))))) {
		if (($s[$i] == "-"[0])) {
			$sign = -1;
		}
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	$result = 0;
	while (($i < $n)) {
		$ch = (is_array($s) ? array_slice($s, $i, (((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)))) - ($i)) : substr($s, $i, (((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)))) - ($i)));
		$d = digit($ch);
		if (($d < 0)) {
			break;
		}
		$result = ((is_array(($result * 10)) && is_array($d)) ? array_merge(($result * 10), $d) : ((is_string(($result * 10)) || is_string($d)) ? (($result * 10) . $d) : (($result * 10) + $d)));
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	$result = ($result * $sign);
	if (($result > 2147483647)) {
		return 2147483647;
	}
	if (($result < (-2147483648))) {
		return -2147483648;
	}
	return $result;
}

function test_example_1() {
	if (!((myAtoi("42") == 42))) { throw new Exception('expect failed'); }
}

function test_example_2() {
	if (!((myAtoi("   -42") == (-42)))) { throw new Exception('expect failed'); }
}

function test_example_3() {
	if (!((myAtoi("4193 with words") == 4193))) { throw new Exception('expect failed'); }
}

function test_example_4() {
	if (!((myAtoi("words and 987") == 0))) { throw new Exception('expect failed'); }
}

function test_example_5() {
	if (!((myAtoi("-91283472332") == (-2147483648)))) { throw new Exception('expect failed'); }
}

test_example_1();
test_example_2();
test_example_3();
test_example_4();
test_example_5();
