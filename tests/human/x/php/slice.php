<?php
_print(array_slice([1,2,3],1,2));
_print(array_slice([1,2,3],0,2));
_print(substr("hello",1,3));

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
