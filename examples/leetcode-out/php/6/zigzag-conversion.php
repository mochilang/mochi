<?php
function mochi_convert($s, $numRows) {
	if ((($numRows <= 1) || ($numRows >= (is_array($s) ? count($s) : strlen($s))))) {
		return $s;
	}
	$rows = [];
	$i = 0;
	while (($i < $numRows)) {
		$rows = ((is_array($rows) && is_array([""])) ? array_merge($rows, [""]) : ((is_string($rows) || is_string([""])) ? ($rows . [""]) : ($rows + [""])));
		$i = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1)));
	}
	$curr = 0;
	$step = 1;
	foreach ((is_string($s) ? str_split($s) : $s) as $ch) {
		$rows[$curr] = ((is_array($rows[$curr]) && is_array($ch)) ? array_merge($rows[$curr], $ch) : ((is_string($rows[$curr]) || is_string($ch)) ? ($rows[$curr] . $ch) : ($rows[$curr] + $ch)));
		if (($curr == 0)) {
			$step = 1;
		} else 
		if (($curr == ($numRows - 1))) {
			$step = -1;
		}
		$curr = ((is_array($curr) && is_array($step)) ? array_merge($curr, $step) : ((is_string($curr) || is_string($step)) ? ($curr . $step) : ($curr + $step)));
	}
	$result = "";
	foreach ((is_string($rows) ? str_split($rows) : $rows) as $row) {
		$result = ((is_array($result) && is_array($row)) ? array_merge($result, $row) : ((is_string($result) || is_string($row)) ? ($result . $row) : ($result + $row)));
	}
	return $result;
}

function mochi_test_example_1() {
	if (!((mochi_convert("PAYPALISHIRING", 3) == "PAHNAPLSIIGYIR"))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_convert("PAYPALISHIRING", 4) == "PINALSIGYAHRPI"))) { throw new Exception('expect failed'); }
}

function mochi_test_single_row() {
	if (!((mochi_convert("A", 1) == "A"))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_single_row();
