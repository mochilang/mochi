<?php
// x: int
$x = 3;
// y: int
$y = 4;
// m: {string: any}
$m = ["a" => $x, "b" => $y];
_print($m["a"], $m["b"]);

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
