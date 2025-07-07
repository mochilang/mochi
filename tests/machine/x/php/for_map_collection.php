<?php
// m: {string: int}
$m = ["a" => 1, "b" => 2];
foreach (array_keys($m) as $k) {
	_print($k);
}

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
