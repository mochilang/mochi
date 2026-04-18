<?php
function getPermutation($n, $k) {
    $digits = [];
    for ($i = 1; $i <= $n; $i++) $digits[] = strval($i);
    $fact = array_fill(0, $n + 1, 1);
    for ($i = 1; $i <= $n; $i++) $fact[$i] = $fact[$i - 1] * $i;
    $k--;
    $out = '';
    for ($rem = $n; $rem >= 1; $rem--) {
        $block = $fact[$rem - 1];
        $idx = intdiv($k, $block);
        $k %= $block;
        $out .= $digits[$idx];
        array_splice($digits, $idx, 1);
    }
    return $out;
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if (!$lines) exit;
$idx = 0;
$t = intval(trim($lines[$idx++]));
$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval(trim($lines[$idx++]));
    $k = intval(trim($lines[$idx++]));
    $out[] = getPermutation($n, $k);
}
echo implode("\n", $out);
?>
