<?php
// a: int
$a = (10 - 3);
// b: int
$b = ((is_array(2) && is_array(2)) ? array_merge(2, 2) : ((is_string(2) || is_string(2)) ? (2 . 2) : (2 + 2)));
_print($a);
_print(($a == 7));
_print(($b < 5));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
