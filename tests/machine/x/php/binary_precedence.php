<?php
_print(((is_array(1) && is_array((2 * 3))) ? array_merge(1, (2 * 3)) : ((is_string(1) || is_string((2 * 3))) ? (1 . (2 * 3)) : (1 + (2 * 3)))));
_print(((((is_array(1) && is_array(2)) ? array_merge(1, 2) : ((is_string(1) || is_string(2)) ? (1 . 2) : (1 + 2)))) * 3));
_print(((is_array((2 * 3)) && is_array(1)) ? array_merge((2 * 3), 1) : ((is_string((2 * 3)) || is_string(1)) ? ((2 * 3) . 1) : ((2 * 3) + 1))));
_print((2 * (((is_array(3) && is_array(1)) ? array_merge(3, 1) : ((is_string(3) || is_string(1)) ? (3 . 1) : (3 + 1))))));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
