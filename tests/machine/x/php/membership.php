<?php
// nums: [int]
$nums = [1, 2, 3];
_print((is_array($nums) ? (array_key_exists(2, $nums) || in_array(2, $nums, true)) : (is_string($nums) ? strpos($nums, strval(2)) !== false : false)));
_print((is_array($nums) ? (array_key_exists(4, $nums) || in_array(4, $nums, true)) : (is_string($nums) ? strpos($nums, strval(4)) !== false : false)));

function _print(...$args) {
    $parts = [];
    foreach ($args as $a) {
        if (is_null($a)) { $parts[] = '<nil>'; }
        elseif (is_array($a) || is_object($a)) { $parts[] = json_encode($a); } else { $parts[] = strval($a); }
    }
    echo implode(' ', $parts), PHP_EOL;
}
