<?php
// s: string
$s = "catch";
_print((is_array($s) ? (array_key_exists("cat", $s) || in_array("cat", $s, true)) : (is_string($s) ? strpos($s, strval("cat")) !== false : false)));
_print((is_array($s) ? (array_key_exists("dog", $s) || in_array("dog", $s, true)) : (is_string($s) ? strpos($s, strval("dog")) !== false : false)));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
