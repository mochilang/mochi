<?php
function expand($s, $left, $right) {
        if (!is_array($s)) {
                $s = str_split($s);
        }
        $l = $left;
        $r = $right;
        $n = (is_array($s) ? count($s) : strlen($s));
	while (((($l >= 0) && $r) < $n)) {
		if (($s[$l] != $s[$r])) {
			break;
		}
		$l = ($l - 1);
		$r = ((is_array($r) && is_array(1)) ? array_merge($r, 1) : ((is_string($r) || is_string(1)) ? ($r . 1) : ($r + 1)));
	}
	return (($r - $l) - 1);
}

function longestPalindrome($s) {
        if (!is_array($s)) {
                $s = str_split($s);
        }
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
		if ((($l > $end) - $start)) {
			$start = (($i - (($l - 1))) / 2);
			$end = (((is_array($i) && is_array($l)) ? array_merge($i, $l) : ((is_string($i) || is_string($l)) ? ($i . $l) : ($i + $l))) / 2);
		}
	}
	return $s[$start];
}

function test_example_1() {
        $ans = longestPalindrome("babad");
        assert($ans == "bab" || $ans == "aba");
}

function test_example_2() {
        assert(longestPalindrome("cbbd") == "bb");
}

function test_single_char() {
        assert(longestPalindrome("a") == "a");
}

function test_two_chars() {
        $ans = longestPalindrome("ac");
        assert($ans == "a" || $ans == "c");
}

function main() {
        assert_options(ASSERT_ACTIVE, 1);
        assert_options(ASSERT_WARNING, 1);
        test_example_1();
        test_example_2();
        test_single_char();
        test_two_chars();
}

main();
