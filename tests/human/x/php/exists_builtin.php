<?php
$data = [1,2];
$flag = false;
foreach ($data as $x) {
    if ($x == 1) { $flag = true; break; }
}
_print($flag ? 'true' : 'false');

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
