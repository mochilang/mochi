<?php
function is_match($s, $p) {
    $i = 0; $j = 0; $star = -1; $match = 0;
    $n = strlen($s); $m = strlen($p);
    while ($i < $n) {
        if ($j < $m && ($p[$j] === '?' || $p[$j] === $s[$i])) { $i++; $j++; }
        elseif ($j < $m && $p[$j] === '*') { $star = $j; $match = $i; $j++; }
        elseif ($star !== -1) { $j = $star + 1; $match++; $i = $match; }
        else return false;
    }
    while ($j < $m && $p[$j] === '*') $j++;
    return $j === $m;
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if (!$lines) exit;
$idx = 0;
$t = intval(trim($lines[$idx++]));
$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval(trim($lines[$idx++]));
    $s = $n > 0 ? $lines[$idx++] : '';
    $m = intval(trim($lines[$idx++]));
    $p = $m > 0 ? $lines[$idx++] : '';
    $out[] = is_match($s, $p) ? 'true' : 'false';
}
echo implode("\n", $out);
?>
