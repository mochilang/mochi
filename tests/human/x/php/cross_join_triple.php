<?php
$nums = [1,2];
$letters = ["A","B"];
$bools = [true,false];
$combos = [];
foreach ($nums as $n) {
    foreach ($letters as $l) {
        foreach ($bools as $b) {
            $combos[] = ["n"=>$n,"l"=>$l,"b"=>$b];
        }
    }
}
_print("--- Cross Join of three lists ---");
foreach ($combos as $c) {
    _print($c['n'], $c['l'], $c['b'] ? 'true' : 'false');
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
