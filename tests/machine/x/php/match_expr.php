<?php
// x: int
$x = 2;
// label: string
$label = (function($_t) {
	if ($_t === 1) return "one";
	if ($_t === 2) return "two";
	if ($_t === 3) return "three";
	return "unknown";
})($x);
_print($label);

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
