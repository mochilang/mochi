<?php
function mochi_isPalindrome($x) {
	if (($x < 0)) {
		return false;
	}
	$s = strval($x);
	$n = (is_array($s) ? count($s) : strlen($s));
	for ($i = 0; $i < ((is_int($n) && is_int(2)) ? intdiv($n, 2) : ($n / 2)); $i++) {
		if (($s[$i] != $s[(($n - 1) - $i)])) {
			return false;
		}
	}
	return true;
}

function mochi_test_example_1() {
	if (!((mochi_isPalindrome(121) == true))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_isPalindrome(-121) == false))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_isPalindrome(10) == false))) { throw new Exception('expect failed'); }
}

function mochi_test_zero() {
	if (!((mochi_isPalindrome(0) == true))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();
mochi_test_zero();
