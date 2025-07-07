<?php
// nums: [int]
$nums = [1, 2, 3];
// letters: [string]
$letters = ["A", "B"];
// pairs: [{string: any}]
$pairs = (function() use ($letters, $nums) {
	$res = [];
	foreach ((is_string($nums) ? str_split($nums) : $nums) as $n) {
		if (!((((is_int($n) && is_int(2)) ? ($n % 2) : fmod($n, 2)) == 0))) { continue; }
		foreach ((is_string($letters) ? str_split($letters) : $letters) as $l) {
			$res[] = ["n" => $n, "l" => $l];
		}
	}
	return $res;
})();
_print("--- Even pairs ---");
foreach ((is_string($pairs) ? str_split($pairs) : $pairs) as $p) {
	_print($p['n'], $p['l']);
}

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
