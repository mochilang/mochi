<?php
function isMatch($s, $p) {
	$m = (is_array($s) ? count($s) : strlen($s));
	$n = (is_array($p) ? count($p) : strlen($p));
	$dp = [];
	$i = 0;
	while (($i <= $m)) {
		$row = [];
		$j = 0;
		while (($j <= $n)) {
			$row = ((is_array($row) && is_array([false])) ? array_merge($row, [false]) : ((is_string($row) || is_string([false])) ? ($row . [false]) : ($row + [false])));
			$j = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
		}
		$dp = ((is_array($dp) && is_array([$row])) ? array_merge($dp, [$row]) : ((is_string($dp) || is_string([$row])) ? ($dp . [$row]) : ($dp + [$row])));
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	$dp[$m][$n] = true;
	$i2 = $m;
	while (($i2 >= 0)) {
		$j2 = ($n - 1);
		while (($j2 >= 0)) {
			$first = false;
			if (($i2 < $m)) {
				if (((($p[$j2] == $s[$i2])) || (($p[$j2] == ".")))) {
					$first = true;
				}
			}
			if (((((is_array($j2) && is_array(1)) ? array_merge($j2, 1) : ((is_string($j2) || is_string(1)) ? ($j2 . 1) : ($j2 + 1))) < $n) && ($p[((is_array($j2) && is_array(1)) ? array_merge($j2, 1) : ((is_string($j2) || is_string(1)) ? ($j2 . 1) : ($j2 + 1)))] == "*"))) {
				if (($dp[$i2][((is_array($j2) && is_array(2)) ? array_merge($j2, 2) : ((is_string($j2) || is_string(2)) ? ($j2 . 2) : ($j2 + 2)))] || (($first && $dp[((is_array($i2) && is_array(1)) ? array_merge($i2, 1) : ((is_string($i2) || is_string(1)) ? ($i2 . 1) : ($i2 + 1)))][$j2])))) {
					$dp[$i2][$j2] = true;
				} else {
					$dp[$i2][$j2] = false;
				}
			} else {
				if (($first && $dp[((is_array($i2) && is_array(1)) ? array_merge($i2, 1) : ((is_string($i2) || is_string(1)) ? ($i2 . 1) : ($i2 + 1)))][((is_array($j2) && is_array(1)) ? array_merge($j2, 1) : ((is_string($j2) || is_string(1)) ? ($j2 . 1) : ($j2 + 1)))])) {
					$dp[$i2][$j2] = true;
				} else {
					$dp[$i2][$j2] = false;
				}
			}
			$j2 = ($j2 - 1);
		}
		$i2 = ($i2 - 1);
	}
	return $dp[0][0];
}

function test_example_1() {
	if (!((isMatch("aa", "a") == false))) { throw new Exception('expect failed'); }
}

function test_example_2() {
	if (!((isMatch("aa", "a*") == true))) { throw new Exception('expect failed'); }
}

function test_example_3() {
	if (!((isMatch("ab", ".*") == true))) { throw new Exception('expect failed'); }
}

function test_example_4() {
	if (!((isMatch("aab", "c*a*b") == true))) { throw new Exception('expect failed'); }
}

function test_example_5() {
	if (!((isMatch("mississippi", "mis*is*p*.") == false))) { throw new Exception('expect failed'); }
}

test_example_1();
test_example_2();
test_example_3();
test_example_4();
test_example_5();
