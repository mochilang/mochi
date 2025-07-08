<?php
function outer($x) {
    function inner($y) {
        return $x + $y;
    }
    return inner(5);
}
_print(outer(3));
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
