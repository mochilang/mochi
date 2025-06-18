<?php
function myAtoi($s) {
	$i = 0;
	$n = count($s);
	while (((($i < $n) && $s[$i]) == " ")) {
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	$sign = 1;
	if ((($i < $n) && (((($s[$i] == "+") || $s[$i]) == "-")))) {
		if (($s[$i] == "-")) {
			$sign = -1;
		}
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	$digits = ["0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9];
	$result = 0;
	while (($i < $n)) {
		$ch = $s[$i];
		if (!((is_array($digits) ? (array_key_exists($ch, $digits) || in_array($ch, $digits, true)) : (is_string($digits) ? strpos($digits, strval($ch)) !== false : false)))) {
			break;
		}
		$d = $digits[$ch];
		$result = ((is_array(($result * 10)) && is_array($d)) ? array_merge(($result * 10), $d) : ((is_string(($result * 10)) || is_string($d)) ? (($result * 10) . $d) : (($result * 10) + $d)));
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	$result = ($result * $sign);
	if (($result > 2147483647)) {
		return 2147483647;
	}
	if (($result < (-2147483648))) {
		return -2147483648;
	}
	return $result;
}

