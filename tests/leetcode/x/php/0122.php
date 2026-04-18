<?php
function maxProfit($prices) {
    $best = 0;
    for ($i = 1; $i < count($prices); $i++) {
        if ($prices[$i] > $prices[$i - 1]) $best += $prices[$i] - $prices[$i - 1];
    }
    return $best;
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
