<?php
function boom() {
    _print("boom");
    return true;
}

_print((1 < 2) && (2 < 3) && (3 < 4));
_print((1 < 2) && (2 > 3) && boom());
_print((1 < 2) && (2 < 3) && (3 > 4) && boom());

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
