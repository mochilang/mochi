<?php
// x: int
$x = 2;
// label: string
$label = match ($x) { 1 => "one", 2 => "two", 3 => "three", default => "unknown" };
_print($label);

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
