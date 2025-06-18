<?php
function twoSum($nums, $target) {
	$n = count($nums);
	for ($i = 0; $i < $n; $i++) {
		for ($j = ((is_array($i) && is_array(1)) ? array_merge($i, 1) : ((is_string($i) || is_string(1)) ? ($i . 1) : ($i + 1))); $j < $n; $j++) {
			if ((((is_array($nums[$i]) && is_array($nums[$j])) ? array_merge($nums[$i], $nums[$j]) : ((is_string($nums[$i]) || is_string($nums[$j])) ? ($nums[$i] . $nums[$j]) : ($nums[$i] + $nums[$j]))) == $target)) {
				return [$i, $j];
			}
		}
	}
	return [-1, -1];
}

function main() {
	$result = twoSum([2, 7, 11, 15], 9);
	echo $result[0], PHP_EOL;
	echo $result[1], PHP_EOL;
}

main();
