<?php
function isPalindrome($x) {
	if (($x < 0)) {
		return false;
	}
	$s = strval($x);
	$n = count($s);
	for ($i = 0; $i < ($n / 2); $i++) {
		if (($s[$i] != $s[(($n - 1) - $i)])) {
			return false;
		}
	}
	return true;
}

