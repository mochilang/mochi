<?php
// nums: [int]
$nums = [1, 2, 3];
// result: [float]
$result = (function() use ($nums) {
	$sum = 0;
	foreach ((is_string($nums) ? str_split($nums) : $nums) as $n) {
		if (!(($n > 1))) { continue; }
		$sum += $n;
	}
	return $sum;
})();
_print($result);

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
