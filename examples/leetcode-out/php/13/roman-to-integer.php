<?php
function mochi_romanToInt($s) {
	$values = ["I" => 1, "V" => 5, "X" => 10, "L" => 50, "C" => 100, "D" => 500, "M" => 1000];
	$total = 0;
	$i = 0;
	$n = (is_array($s) ? count($s) : strlen($s));
	while (($i < $n)) {
		$curr = $values[$s[$i]];
		if ((((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1))) < $n)) {
			$next = $values[$s[((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)))]];
			if (($curr < $next)) {
				$total = (((is_array($total) && is_array($next)) ? array_merge($total, $next) : ((is_string($total) || is_string($next)) ? ($total . $next) : ($total + $next))) - $curr);
				$i = ((is_array($i) && is_array(2)) ? array_merge($i, 2) : ((is_string($i) || is_string(2)) ? ($i . 2) : ($i + 2)));
				continue;
			}
		}
		$total = ((is_array($total) && is_array($curr)) ? array_merge($total, $curr) : ((is_string($total) || is_string($curr)) ? ($total . $curr) : ($total + $curr)));
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	return $total;
}

function mochi_test_example_1() {
	if (!((mochi_romanToInt("III") == 3))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_romanToInt("LVIII") == 58))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_romanToInt("MCMXCIV") == 1994))) { throw new Exception('expect failed'); }
}

function mochi_test_subtractive() {
	if (!((mochi_romanToInt("IV") == 4))) { throw new Exception('expect failed'); }
	if (!((mochi_romanToInt("IX") == 9))) { throw new Exception('expect failed'); }
}

function mochi_test_tens() {
	if (!((mochi_romanToInt("XL") == 40))) { throw new Exception('expect failed'); }
	if (!((mochi_romanToInt("XC") == 90))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();
mochi_test_subtractive();
mochi_test_tens();
