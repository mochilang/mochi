<?php
function reverse($x) {
	$sign = 1;
	$n = $x;
	if (($n < 0)) {
		$sign = -1;
		$n = -$n;
	}
	$rev = 0;
	while (($n != 0)) {
		$digit = ($n % 10);
		$rev = ((is_array(($rev * 10)) && is_array($digit)) ? array_merge(($rev * 10), $digit) : ((is_string(($rev * 10)) || is_string($digit)) ? (($rev * 10) . $digit) : (($rev * 10) + $digit)));
		$n = ($n / 10);
	}
	$rev = ($rev * $sign);
	if (((($rev < ((-2147483647 - 1))) || $rev) > 2147483647)) {
		return 0;
	}
	return $rev;
}

