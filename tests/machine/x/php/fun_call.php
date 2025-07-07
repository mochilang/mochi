<?php
/**
 * @param int $a
 * @param int $b
 * @return int
 */
function mochi_add($a, $b) {
	return ((is_array($a) && is_array($b)) ? array_merge($a, $b) : ((is_string($a) || is_string($b)) ? ($a . $b) : ($a + $b)));
}

_print(mochi_add(2, 3));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
