<?php
function isPalindrome($x) {
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

function test_example_1() {
	if (!((isPalindrome(121) == true))) { throw new Exception('expect failed'); }
}

function test_example_2() {
	if (!((isPalindrome(-121) == false))) { throw new Exception('expect failed'); }
}

function test_example_3() {
	if (!((isPalindrome(10) == false))) { throw new Exception('expect failed'); }
}

function test_zero() {
	if (!((isPalindrome(0) == true))) { throw new Exception('expect failed'); }
}

test_example_1();
test_example_2();
test_example_3();
test_zero();
