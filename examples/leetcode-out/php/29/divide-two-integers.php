<?php
function mochi_divide($dividend, $divisor) {
	if ((($dividend == ((-2147483647 - 1))) && ($divisor == (-1)))) {
		return 2147483647;
	}
	$negative = false;
	if (($dividend < 0)) {
		$negative = !$negative;
		$dividend = -$dividend;
	}
	if (($divisor < 0)) {
		$negative = !$negative;
		$divisor = -$divisor;
	}
	$quotient = 0;
	while (($dividend >= $divisor)) {
		$temp = $divisor;
		$multiple = 1;
		while (($dividend >= ((is_array($temp) && is_array($temp)) ? array_merge($temp, $temp) : ((is_string($temp) || is_string($temp)) ? ($temp . $temp) : ($temp + $temp))))) {
			$temp = ((is_array($temp) && is_array($temp)) ? array_merge($temp, $temp) : ((is_string($temp) || is_string($temp)) ? ($temp . $temp) : ($temp + $temp)));
			$multiple = ((is_array($multiple) && is_array($multiple)) ? array_merge($multiple, $multiple) : ((is_string($multiple) || is_string($multiple)) ? ($multiple . $multiple) : ($multiple + $multiple)));
		}
		$dividend = ($dividend - $temp);
		$quotient = ((is_array($quotient) && is_array($multiple)) ? array_merge($quotient, $multiple) : ((is_string($quotient) || is_string($multiple)) ? ($quotient . $multiple) : ($quotient + $multiple)));
	}
	if ($negative) {
		$quotient = -$quotient;
	}
	if (($quotient > 2147483647)) {
		return 2147483647;
	}
	if (($quotient < ((-2147483647 - 1)))) {
		return -2147483648;
	}
	return $quotient;
}

function mochi_test_example_1() {
	if (!((mochi_divide(10, 3) == 3))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_divide(7, -3) == (-2)))) { throw new Exception('expect failed'); }
}

function mochi_test_overflow() {
	if (!((mochi_divide(-2147483648, -1) == 2147483647))) { throw new Exception('expect failed'); }
}

function mochi_test_divide_by_1() {
	if (!((mochi_divide(12345, 1) == 12345))) { throw new Exception('expect failed'); }
}

function mochi_test_negative_result() {
	if (!((mochi_divide(-15, 2) == (-7)))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_overflow();
mochi_test_divide_by_1();
mochi_test_negative_result();
