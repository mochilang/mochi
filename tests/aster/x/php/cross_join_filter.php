<?php
$nums = [1, 2, 3];
$letters = ["A", "B"];
;
foreach ($nums as $n) {
    foreach ($letters as $l) {
        if ($n % 2 == 0) {
             = ["n" => $n, "l" => $l];
        }
    }
}
echo "--- Even pairs ---", PHP_EOL;
foreach ($pairs as $p) {
    echo  . (is_float($p["l"]) ? json_encode($p["l"], 1344) : $p["l"]), PHP_EOL;
}
