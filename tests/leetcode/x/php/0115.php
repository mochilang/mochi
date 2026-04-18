<?php
function solve($s, $t) {
    $dp = array_fill(0, strlen($t) + 1, 0);
    $dp[0] = 1;
    for ($i = 0; $i < strlen($s); $i++) {
        for ($j = strlen($t); $j >= 1; $j--) {
            if ($s[$i] === $t[$j - 1]) $dp[$j] += $dp[$j - 1];
        }
    }
    return $dp[strlen($t)];
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if ($lines === false || count($lines) === 0) exit(0);
$tc = intval(trim($lines[0]));
$out = [];
for ($i = 0; $i < $tc; $i++) {
    $out[] = strval(solve($lines[1 + 2 * $i], $lines[2 + 2 * $i]));
}
echo implode("\n", $out);
?>
