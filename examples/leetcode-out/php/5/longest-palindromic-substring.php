<?php
function expand($s, $left, $right) {
	$l = $left;
	$r = $right;
	$n = (is_array($s) ? count($s) : strlen($s));
	while ((($l >= 0) && ($r < $n))) {
		if (($s[$l] != $s[$r])) {
			break;
		}
		$l = ($l - 1);
		$r = ((is_array($r) && is_array(1)) ? array_merge($r, 1) : ((is_string($r) || is_string(1)) ? ($r . 1) : ($r + 1)));
	}
	return (($r - $l) - 1);
}

function longestPalindrome($s) {
	if (((is_array($s) ? count($s) : strlen($s)) <= 1)) {
		return $s;
	}
	$start = 0;
	$end = 0;
	$n = (is_array($s) ? count($s) : strlen($s));
	for ($i = 0; $i < $n; $i++) {
		$len1 = expand($s, $i, $i);
		$len2 = expand($s, $i, ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1))));
		$l = $len1;
		if (($len2 > $len1)) {
			$l = $len2;
		}
		if (($l > (($end - $start)))) {
			$start = ($i - (((is_int((($l - 1))) && is_int(2)) ? intdiv((($l - 1)), 2) : ((($l - 1)) / 2))));
			$end = ((is_array($i) && is_array((((is_int($l) && is_int(2)) ? intdiv($l, 2) : ($l / 2))))) ? array_merge($i, (((is_int($l) && is_int(2)) ? intdiv($l, 2) : ($l / 2)))) : ((is_string($i) || is_string((((is_int($l) && is_int(2)) ? intdiv($l, 2) : ($l / 2))))) ? ($i . (((is_int($l) && is_int(2)) ? intdiv($l, 2) : ($l / 2)))) : ($i + (((is_int($l) && is_int(2)) ? intdiv($l, 2) : ($l / 2))))));
		}
	}
	return (is_array($s) ? array_slice($s, $start, (((is_array($end) && is_array(1)) ? array_merge($end, 1) : ((is_string($end) || is_string(1)) ? ($end . 1) : ($end + 1)))) - ($start)) : substr($s, $start, (((is_array($end) && is_array(1)) ? array_merge($end, 1) : ((is_string($end) || is_string(1)) ? ($end . 1) : ($end + 1)))) - ($start)));
}

function test_example_1() {
	$ans = longestPalindrome("babad");
	if (!((($ans == "bab") || ($ans == "aba")))) { throw new Exception('expect failed'); }
}

function test_example_2() {
	if (!((longestPalindrome("cbbd") == "bb"))) { throw new Exception('expect failed'); }
}

function test_single_char() {
	if (!((longestPalindrome("a") == "a"))) { throw new Exception('expect failed'); }
}

function test_two_chars() {
	$ans = longestPalindrome("ac");
	if (!((($ans == "a") || ($ans == "c")))) { throw new Exception('expect failed'); }
}

test_example_1();
test_example_2();
test_single_char();
test_two_chars();
