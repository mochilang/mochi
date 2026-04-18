<?php
function maxArea($h) {
    $left = 0; $right = count($h) - 1; $best = 0;
    while ($left < $right) {
        $height = min($h[$left], $h[$right]);
        $best = max($best, ($right - $left) * $height);
        if ($h[$left] < $h[$right]) $left++; else $right--;
    }
    return $best;
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if ($lines === false || count($lines) === 0) exit(0);
$t = intval(trim($lines[0]));
$idx = 1; $out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval(trim($lines[$idx++]));
    $h = [];
    for ($i = 0; $i < $n; $i++) $h[] = intval(trim($lines[$idx++]));
    $out[] = strval(maxArea($h));
}
echo implode("\n", $out);
?>
