<?php
$nums = [1, 2, 3];
$letters = ["A", "B"];
$pairs = (function() use ($letters, $nums) {
    $result = [];
    foreach ($nums as $n) {
        foreach ($letters as $l) {
            if ($n % 2 == 0) {
                $result[] = ["n" => $n, "l" => $l];
            }
        }
    }
    return $result;
})();
echo "--- Even pairs ---", PHP_EOL;
foreach ($pairs as $p) {
    var_dump($p['n'], $p['l']);
}
?>
