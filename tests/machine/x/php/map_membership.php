<?php
// m: {string: int}
$m = ["a" => 1, "b" => 2];
_print((is_array($m) ? (array_key_exists("a", $m) || in_array("a", $m, true)) : (is_string($m) ? strpos($m, strval("a")) !== false : false)));
_print((is_array($m) ? (array_key_exists("c", $m) || in_array("c", $m, true)) : (is_string($m) ? strpos($m, strval("c")) !== false : false)));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
