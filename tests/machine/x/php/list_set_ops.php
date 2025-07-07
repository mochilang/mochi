<?php
_print(array_values(array_unique(array_merge([1, 2], [2, 3]), SORT_REGULAR)));
_print(array_values(array_diff([1, 2, 3], [2])));
_print(array_values(array_intersect([1, 2, 3], [2, 4])));
_print((is_array(array_merge([1, 2], [2, 3])) ? count(array_merge([1, 2], [2, 3])) : strlen(array_merge([1, 2], [2, 3]))));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
