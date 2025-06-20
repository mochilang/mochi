<?php
function mochi_isValid($s) {
	$stack = [];
	$n = (is_array($s) ? count($s) : strlen($s));
	for ($i = 0; $i < $n; $i++) {
		$c = $s[$i];
		if (($c == "(")) {
			$stack = ((is_array($stack) && is_array([")"])) ? array_merge($stack, [")"]) : ((is_string($stack) || is_string([")"])) ? ($stack . [")"]) : ($stack + [")"])));
		} else 
		if (($c == "[")) {
			$stack = ((is_array($stack) && is_array(["]"])) ? array_merge($stack, ["]"]) : ((is_string($stack) || is_string(["]"])) ? ($stack . ["]"]) : ($stack + ["]"])));
		} else 
		if (($c == "{")) {
			$stack = ((is_array($stack) && is_array(["}"])) ? array_merge($stack, ["}"]) : ((is_string($stack) || is_string(["}"])) ? ($stack . ["}"]) : ($stack + ["}"])));
		} else {
			if (((is_array($stack) ? count($stack) : strlen($stack)) == 0)) {
				return false;
			}
			$top = $stack[((is_array($stack) ? count($stack) : strlen($stack)) - 1)];
			if (($top != $c)) {
				return false;
			}
			$stack = (is_array($stack) ? array_slice($stack, 0, (((is_array($stack) ? count($stack) : strlen($stack)) - 1)) - (0)) : substr($stack, 0, (((is_array($stack) ? count($stack) : strlen($stack)) - 1)) - (0)));
		}
	}
	return ((is_array($stack) ? count($stack) : strlen($stack)) == 0);
}

function mochi_test_example_1() {
	if (!((mochi_isValid("()") == true))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_isValid("()[]{}") == true))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_isValid("(]") == false))) { throw new Exception('expect failed'); }
}

function mochi_test_example_4() {
	if (!((mochi_isValid("([)]") == false))) { throw new Exception('expect failed'); }
}

function mochi_test_example_5() {
	if (!((mochi_isValid("{[]}") == true))) { throw new Exception('expect failed'); }
}

function mochi_test_empty_string() {
	if (!((mochi_isValid("") == true))) { throw new Exception('expect failed'); }
}

function mochi_test_single_closing() {
	if (!((mochi_isValid("]") == false))) { throw new Exception('expect failed'); }
}

function mochi_test_unmatched_open() {
	if (!((mochi_isValid("((") == false))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();
mochi_test_example_4();
mochi_test_example_5();
mochi_test_empty_string();
mochi_test_single_closing();
mochi_test_unmatched_open();
