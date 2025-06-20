<?php
function mochi_letterCombinations($digits) {
	if (((is_array($digits) ? count($digits) : strlen($digits)) == 0)) {
		return [];
	}
	$mapping = ["2" => ["a", "b", "c"], "3" => ["d", "e", "f"], "4" => ["g", "h", "i"], "5" => ["j", "k", "l"], "6" => ["m", "n", "o"], "7" => ["p", "q", "r", "s"], "8" => ["t", "u", "v"], "9" => ["w", "x", "y", "z"]];
	$result = [""];
	foreach ((is_string($digits) ? str_split($digits) : $digits) as $d) {
		if (!((is_array($mapping) ? (array_key_exists($d, $mapping) || in_array($d, $mapping, true)) : (is_string($mapping) ? strpos($mapping, strval($d)) !== false : false)))) {
			continue;
		}
		$letters = $mapping[$d];
		$next = (function() use ($letters, $result) {
	$res = [];
	foreach ((is_string($result) ? str_split($result) : $result) as $p) {
		foreach ((is_string($letters) ? str_split($letters) : $letters) as $ch) {
			$res[] = ((is_array($p) && is_array($ch)) ? array_merge($p, $ch) : ((is_string($p) || is_string($ch)) ? ($p . $ch) : ($p + $ch)));
		}
	}
	return $res;
})();
		$result = $next;
	}
	return $result;
}

function mochi_test_example_1() {
	if (!((mochi_letterCombinations("23") == ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"]))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_letterCombinations("") == []))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_letterCombinations("2") == ["a", "b", "c"]))) { throw new Exception('expect failed'); }
}

function mochi_test_single_seven() {
	if (!((mochi_letterCombinations("7") == ["p", "q", "r", "s"]))) { throw new Exception('expect failed'); }
}

function mochi_test_mix() {
	if (!((mochi_letterCombinations("79") == ["pw", "px", "py", "pz", "qw", "qx", "qy", "qz", "rw", "rx", "ry", "rz", "sw", "sx", "sy", "sz"]))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();
mochi_test_single_seven();
mochi_test_mix();
