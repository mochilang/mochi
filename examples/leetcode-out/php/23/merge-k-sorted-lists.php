<?php
function mochi_mergeKLists($lists) {
	$k = (is_array($lists) ? count($lists) : strlen($lists));
	$indices = [];
	$i = 0;
	while (($i < $k)) {
		$indices = ((is_array($indices) && is_array([0])) ? array_merge($indices, [0]) : ((is_string($indices) || is_string([0])) ? ($indices . [0]) : ($indices + [0])));
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	$result = [];
	while (true) {
		$best = 0;
		$bestList = -1;
		$found = false;
		$j = 0;
		while (($j < $k)) {
			$idx = $indices[$j];
			if (($idx < (is_array($lists[$j]) ? count($lists[$j]) : strlen($lists[$j])))) {
				$val = $lists[$j][$idx];
				if ((!$found || ($val < $best))) {
					$best = $val;
					$bestList = $j;
					$found = true;
				}
			}
			$j = ((is_array($j) && is_array(1)) ? array_merge($j, 1) : ((is_string($j) || is_string(1)) ? ($j . 1) : ($j + 1)));
		}
		if (!$found) {
			break;
		}
		$result = ((is_array($result) && is_array([$best])) ? array_merge($result, [$best]) : ((is_string($result) || is_string([$best])) ? ($result . [$best]) : ($result + [$best])));
		$indices[$bestList] = ((is_array($indices[$bestList]) && is_array(1)) ? array_merge($indices[$bestList], 1) : ((is_string($indices[$bestList]) || is_string(1)) ? ($indices[$bestList] . 1) : ($indices[$bestList] + 1)));
	}
	return $result;
}

function mochi_test_example_1() {
	if (!((mochi_mergeKLists([[1, 4, 5], [1, 3, 4], [2, 6]]) == [1, 1, 2, 3, 4, 4, 5, 6]))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_mergeKLists([]) == []))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_mergeKLists([[]]) == []))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();
