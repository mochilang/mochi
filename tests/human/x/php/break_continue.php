<?php
$numbers = [1,2,3,4,5,6,7,8,9];
foreach ($numbers as $n) {
    if ($n % 2 == 0) {
        continue;
    }
    if ($n > 7) {
        break;
    }
    _print("odd number:", $n);
}

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
