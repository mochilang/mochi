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
var_dump("--- Even pairs ---");
foreach ($pairs as $p) {
    var_dump($p['n'], $p['l']);
}
?>
