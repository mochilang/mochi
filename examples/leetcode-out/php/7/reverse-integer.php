<?php
function mochi_reverse($x) {
	$sign = 1;
	$n = $x;
	if (($n < 0)) {
		$sign = -1;
		$n = -$n;
	}
	$rev = 0;
	while (($n != 0)) {
		$digit = ((is_int($n) && is_int(10)) ? ($n % 10) : fmod($n, 10));
		$rev = ((is_array(($rev * 10)) && is_array($digit)) ? array_merge(($rev * 10), $digit) : ((is_string(($rev * 10)) || is_string($digit)) ? (($rev * 10) . $digit) : (($rev * 10) + $digit)));
		$n = ((is_int($n) && is_int(10)) ? intdiv($n, 10) : ($n / 10));
	}
	$rev = ($rev * $sign);
	if ((($rev < ((-2147483647 - 1))) || ($rev > 2147483647))) {
		return 0;
	}
	return $rev;
}

function mochi_test_example_1() {
	if (!((mochi_reverse(123) == 321))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_reverse(-123) == (-321)))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_reverse(120) == 21))) { throw new Exception('expect failed'); }
}

function mochi_test_overflow() {
	if (!((mochi_reverse(1534236469) == 0))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();
mochi_test_overflow();
