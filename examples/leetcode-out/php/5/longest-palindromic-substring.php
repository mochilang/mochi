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
		if (($l > (($end - $start)))) {
			$start = ($i - (((($l - 1)) / 2)));
			$end = ((is_array($i) && is_array((($l / 2)))) ? array_merge($i, (($l / 2))) : ((is_string($i) || is_string((($l / 2)))) ? ($i . (($l / 2))) : ($i + (($l / 2)))));
		}
	}
	$res = "";
	$k = $start;
	while (($k <= $end)) {
		$res = ((is_array($res) && is_array($s[$k])) ? array_merge($res, $s[$k]) : ((is_string($res) || is_string($s[$k])) ? ($res . $s[$k]) : ($res + $s[$k])));
		$k = ((is_array($k) && is_array(1)) ? array_merge($k, 1) : ((is_string($k) || is_string(1)) ? ($k . 1) : ($k + 1)));
	}
	return $res;
}

