<?php
_print([1, 2] union [2, 3]);
_print([1, 2, 3] except [2]);
_print([1, 2, 3] intersect [2, 4]);
_print(count([1, 2] union [2, 3]));
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
