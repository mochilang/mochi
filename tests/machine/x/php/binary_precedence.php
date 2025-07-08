<?php
_print(1 + 2 * 3);
_print((1 + 2) * 3);
_print(2 * 3 + 1);
_print(2 * (3 + 1));
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
