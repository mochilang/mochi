<?php
function expand($s, $left, $right) {
	$l = $left;
	$r = $right;
	$n = count($s);
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
	if ((count($s) <= 1)) {
		return $s;
	}
	$start = 0;
	$end = 0;
	$n = count($s);
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

