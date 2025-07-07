<?php
$nums = [1,2,3];
$result = 0;
foreach ($nums as $n) {
    if ($n > 1) $result += $n;
}
_print($result);

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
