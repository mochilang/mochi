<?php
function firstMissingPositive($nums) {
    $n = count($nums);
    $i = 0;
    while ($i < $n) {
        $v = $nums[$i];
        if ($v >= 1 && $v <= $n && $nums[$v - 1] != $v) {
            $tmp = $nums[$i];
            $nums[$i] = $nums[$v - 1];
            $nums[$v - 1] = $tmp;
        } else {
            $i++;
        }
    }
    for ($i = 0; $i < $n; $i++) {
        if ($nums[$i] != $i + 1) return $i + 1;
    }
    return $n + 1;
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if (!$lines) exit;
$idx = 0;
$t = intval(trim($lines[$idx++]));
$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval(trim($lines[$idx++]));
    $nums = [];
    for ($i = 0; $i < $n; $i++) $nums[] = intval(trim($lines[$idx++]));
    $out[] = strval(firstMissingPositive($nums));
}
echo implode("\n", $out);
?>
