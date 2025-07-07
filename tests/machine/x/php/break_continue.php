<?php
// numbers: [int]
$numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9];
foreach ((is_string($numbers) ? str_split($numbers) : $numbers) as $n) {
	if ((((is_int($n) && is_int(2)) ? ($n % 2) : fmod($n, 2)) == 0)) {
		continue;
	}
	if (($n > 7)) {
		break;
	}
	_print("odd number:", $n);
}

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
