<?php
function mochi_generateParenthesis($n) {
	$result = [];
	$backtrack = null;
	$backtrack = function ($current, $open, $close) use (&$n, &$result, &$backtrack) {
		if (((is_array($current) ? count($current) : strlen($current)) == ($n * 2))) {
			$result = ((is_array($result) && is_array([$current])) ? array_merge($result, [$current]) : ((is_string($result) || is_string([$current])) ? ($result . [$current]) : ($result + [$current])));
		} else {
			if (($open < $n)) {
				$backtrack(((is_array($current) && is_array("(")) ? array_merge($current, "(") : ((is_string($current) || is_string("(")) ? ($current . "(") : ($current + "("))), ((is_array($open) && is_array(1)) ? array_merge($open, 1) : ((is_string($open) || is_string(1)) ? ($open . 1) : ($open + 1))), $close);
			}
			if (($close < $open)) {
				$backtrack(((is_array($current) && is_array(")")) ? array_merge($current, ")") : ((is_string($current) || is_string(")")) ? ($current . ")") : ($current + ")"))), $open, ((is_array($close) && is_array(1)) ? array_merge($close, 1) : ((is_string($close) || is_string(1)) ? ($close . 1) : ($close + 1))));
			}
		}
	};
	$backtrack("", 0, 0);
	return $result;
}

function mochi_test_example_1() {
	if (!((mochi_generateParenthesis(3) == ["((()))", "(()())", "(())()", "()(())", "()()()"]))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_generateParenthesis(1) == ["()"]))) { throw new Exception('expect failed'); }
}

function mochi_test_two_pairs() {
	if (!((mochi_generateParenthesis(2) == ["(())", "()()"]))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_two_pairs();
