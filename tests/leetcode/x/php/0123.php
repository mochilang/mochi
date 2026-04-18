<?php
function maxProfit($prices) {
    $buy1 = -1000000000; $sell1 = 0; $buy2 = -1000000000; $sell2 = 0;
    for ($i = 0; $i < count($prices); $i++) {
        $p = $prices[$i];
        $buy1 = max($buy1, -$p);
        $sell1 = max($sell1, $buy1 + $p);
        $buy2 = max($buy2, $sell1 - $p);
        $sell2 = max($sell2, $buy2 + $p);
    }
    return $sell2;
}

$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if ($lines === false || count($lines) === 0) exit(0);
$t = intval(trim($lines[0]));
$idx = 1; $out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval(trim($lines[$idx++]));
    $prices = [];
    for ($i = 0; $i < $n; $i++) $prices[] = intval(trim($lines[$idx++]));
    $out[] = strval(maxProfit($prices));
}
echo implode("\n", $out);
?>
