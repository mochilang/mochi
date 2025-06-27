<?php
function mochi_strStr($haystack, $needle) {
	$n = (is_array($haystack) ? count($haystack) : strlen($haystack));
	$m = (is_array($needle) ? count($needle) : strlen($needle));
	if (($m == 0)) {
		return 0;
	}
	if (($m > $n)) {
		return -1;
	}
	for ($i = 0; $i < ((is_array(($n - $m)) && is_array(1)) ? array_merge(($n - $m), 1) : ((is_string(($n - $m)) || is_string(1)) ? (($n - $m) . 1) : (($n - $m) + 1))); $i++) {
		$j = 0;
		while (($j < $m)) {
			if (($haystack[((is_array($i) && is_array($j)) ? array_merge($i, $j) : ((is_string($i) || is_string($j)) ? ($i . $j) : ($i + $j)))] != $needle[$j])) {
				break;
			}
			$j = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
		}
		if (($j == $m)) {
			return $i;
		}
	}
	return -1;
}

function mochi_test_example_1() {
	if (!((mochi_strStr("sadbutsad", "sad") == 0))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_strStr("leetcode", "leeto") == (-1)))) { throw new Exception('expect failed'); }
}

function mochi_test_empty_needle() {
	if (!((mochi_strStr("abc", "") == 0))) { throw new Exception('expect failed'); }
}

function mochi_test_needle_at_end() {
	if (!((mochi_strStr("hello", "lo") == 3))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_empty_needle();
mochi_test_needle_at_end();
