<?php
$m = ["a" => 1, "b" => 2];
_print(in_array("a", $m));
_print(in_array("c", $m));
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
