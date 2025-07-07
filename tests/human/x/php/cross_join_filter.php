<?php
$nums = [1,2,3];
$letters = ["A","B"];
$pairs = [];
foreach ($nums as $n) {
    foreach ($letters as $l) {
        if ($n % 2 == 0) {
            $pairs[] = ["n" => $n, "l" => $l];
        }
    }
}
_print("--- Even pairs ---");
foreach ($pairs as $p) {
    _print($p['n'], $p['l']);
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
