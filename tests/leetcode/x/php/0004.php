<?php
function median($a, $b) {
    $m = []; $i = 0; $j = 0;
    while ($i < count($a) && $j < count($b)) {
        if ($a[$i] <= $b[$j]) $m[] = $a[$i++]; else $m[] = $b[$j++];
    }
    while ($i < count($a)) $m[] = $a[$i++];
    while ($j < count($b)) $m[] = $b[$j++];
    $n = count($m);
    if ($n % 2 == 1) return $m[intdiv($n, 2)];
    return ($m[intdiv($n, 2) - 1] + $m[intdiv($n, 2)]) / 2.0;
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if ($lines === false || count($lines) === 0) exit(0);
$t = intval(trim($lines[0]));
$idx = 1; $out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval(trim($lines[$idx++])); $a = [];
    for ($i = 0; $i < $n; $i++) $a[] = intval(trim($lines[$idx++]));
    $m = intval(trim($lines[$idx++])); $b = [];
    for ($i = 0; $i < $m; $i++) $b[] = intval(trim($lines[$idx++]));
    $out[] = number_format(median($a, $b), 1, '.', '');
}
echo implode("\n", $out);
?>
