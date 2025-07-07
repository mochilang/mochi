<?php
$prefix = "fore";
$s1 = "forest";
_print(substr($s1, 0, strlen($prefix)) == $prefix);
$s2 = "desert";
_print(substr($s2, 0, strlen($prefix)) == $prefix);

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
