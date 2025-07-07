<?php
$nums = [1, 2, 3];
$avg = count($nums) ? array_sum($nums) / count($nums) : 0;
_print($avg);

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
?>
