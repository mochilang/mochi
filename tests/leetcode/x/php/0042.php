<?php
function trapWater($height) {
    $left = 0;
    $right = count($height) - 1;
    $leftMax = 0;
    $rightMax = 0;
    $water = 0;
    while ($left <= $right) {
        if ($leftMax <= $rightMax) {
            if ($height[$left] < $leftMax) $water += $leftMax - $height[$left];
            else $leftMax = $height[$left];
            $left++;
        } else {
            if ($height[$right] < $rightMax) $water += $rightMax - $height[$right];
            else $rightMax = $height[$right];
            $right--;
        }
    }
    return $water;
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if (!$lines) exit;
$idx = 0;
$t = intval(trim($lines[$idx++]));
$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval(trim($lines[$idx++]));
    $arr = [];
    for ($i = 0; $i < $n; $i++) $arr[] = intval(trim($lines[$idx++]));
    $out[] = strval(trapWater($arr));
}
echo implode("\n", $out);
?>
