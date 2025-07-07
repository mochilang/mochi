<?php
_print((6 * 7));
_print(((is_int(7) && is_int(2)) ? intdiv(7, 2) : (7 / 2)));
_print(((is_int(7) && is_int(2)) ? (7 % 2) : fmod(7, 2)));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
