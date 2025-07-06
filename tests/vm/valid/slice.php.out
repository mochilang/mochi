<?php
_print((is_array([1, 2, 3]) ? array_slice([1, 2, 3], 1, (3) - (1)) : substr([1, 2, 3], 1, (3) - (1))));
_print((is_array([1, 2, 3]) ? array_slice([1, 2, 3], 0, (2) - (0)) : substr([1, 2, 3], 0, (2) - (0))));
_print((is_array("hello") ? array_slice("hello", 1, (4) - (1)) : substr("hello", 1, (4) - (1))));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
