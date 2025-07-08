<?php
$a = 10 - 3;
$b = 2 + 2;
_print($a);
_print($a == 7);
_print($b < 5);
function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_array($a) || is_object($a)) {
            $parts[] = json_encode($a);
        } else {
            $parts[] = strval($a);
        }
    }
    echo implode(' ', $parts), PHP_EOL;
}
