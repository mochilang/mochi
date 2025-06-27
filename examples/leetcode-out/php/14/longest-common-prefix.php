<?php
function mochi_longestCommonPrefix($strs) {
	if (((is_array($strs) ? count($strs) : strlen($strs)) == 0)) {
		return "";
	}
	$prefix = $strs[0];
	for ($i = 1; $i < (is_array($strs) ? count($strs) : strlen($strs)); $i++) {
		$j = 0;
		$current = $strs[$i];
		while ((($j < (is_array($prefix) ? count($prefix) : strlen($prefix))) && ($j < (is_array($current) ? count($current) : strlen($current))))) {
			if (($prefix[$j] != $current[$j])) {
				break;
			}
			$j = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
		}
		$prefix = (is_array($prefix) ? array_slice($prefix, 0, ($j) - (0)) : substr($prefix, 0, ($j) - (0)));
		if (($prefix == "")) {
			break;
		}
	}
	return $prefix;
}

function mochi_test_example_1() {
	if (!((mochi_longestCommonPrefix(["flower", "flow", "flight"]) == "fl"))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_longestCommonPrefix(["dog", "racecar", "car"]) == ""))) { throw new Exception('expect failed'); }
}

function mochi_test_single_string() {
	if (!((mochi_longestCommonPrefix(["single"]) == "single"))) { throw new Exception('expect failed'); }
}

function mochi_test_no_common_prefix() {
	if (!((mochi_longestCommonPrefix(["a", "b", "c"]) == ""))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_single_string();
mochi_test_no_common_prefix();
