<?php
function mochi_intToRoman($num) {
	$values = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
	$symbols = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"];
	$result = "";
	$i = 0;
	while (($num > 0)) {
		while (($num >= $values[$i])) {
			$result = ((is_array($result) && is_array($symbols[$i])) ? array_merge($result, $symbols[$i]) : ((is_string($result) || is_string($symbols[$i])) ? ($result . $symbols[$i]) : ($result + $symbols[$i])));
			$num = ($num - $values[$i]);
		}
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	return $result;
}

function mochi_test_example_1() {
	if (!((mochi_intToRoman(3) == "III"))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_intToRoman(58) == "LVIII"))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_intToRoman(1994) == "MCMXCIV"))) { throw new Exception('expect failed'); }
}

function mochi_test_small_numbers() {
	if (!((mochi_intToRoman(4) == "IV"))) { throw new Exception('expect failed'); }
	if (!((mochi_intToRoman(9) == "IX"))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();
mochi_test_small_numbers();
