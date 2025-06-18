<?php
function lengthOfLongestSubstring($s) {
	$n = count($s);
	$start = 0;
	$best = 0;
	$i = 0;
	while (($i < $n)) {
		$j = $start;
		while (($j < $i)) {
			if (($s[$j] == $s[$i])) {
				$start = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
				break;
			}
			$j = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
		}
		$length = ((is_array(($i - $start)) && is_array(1)) ? array_merge(($i - $start), 1) : ((is_string(($i - $start)) || is_string(1)) ? (($i - $start) . 1) : (($i - $start) + 1)));
		if (($length > $best)) {
			$best = $length;
		}
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	return $best;
}

