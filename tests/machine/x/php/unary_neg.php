<?php
_print(-3);
_print(((is_array(5) && is_array((-2))) ? array_merge(5, (-2)) : ((is_string(5) || is_string((-2))) ? (5 . (-2)) : (5 + (-2)))));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
