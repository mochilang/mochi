<?php
$nums = [1, 2, 3];
$letters = ["A", "B"];
$pairs = (function() {
    $result = [];
    foreach ($nums as $n) {
        foreach ($letters as $l) {
            if ($n % 2 == 0) {
                $result[] = [$n => $n, $l => $l];
            }
        }
    }
    return $result;
})();
var_dump("--- Even pairs ---");
foreach ($pairs as $p) {
    var_dump($p->n, $p->l);
}
?>
