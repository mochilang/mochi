<?php
function makeAdder($n) {
    return function($x) use ($n) {
        return $x + $n;
    };
}

$add10 = makeAdder(10);
_print($add10(7));

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
