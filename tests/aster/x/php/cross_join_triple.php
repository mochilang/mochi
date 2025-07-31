<?php
$nums = [1, 2];
$letters = ["A", "B"];
;
;
foreach ($nums as $n) {
    foreach ($letters as $l) {
        foreach ($bools as $b) {
             = ["n" => $n, "l" => $l, "b" => $b];
        }
    }
}
echo "--- Cross Join of three lists ---", PHP_EOL;
foreach ($combos as $c) {
    echo  . (is_float($c["b"]) ? json_encode($c["b"], 1344) : $c["b"]), PHP_EOL;
}
