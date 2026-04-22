<?php
function solve($vals, $ok) {
    $best = -1000000000;
    $n = count($vals);
    $dfs = function($i) use (&$dfs, &$vals, &$ok, &$best, $n) {
        if ($i >= $n || !$ok[$i]) return 0;
        $left = max(0, $dfs(2 * $i + 1));
        $right = max(0, $dfs(2 * $i + 2));
        $best = max($best, $vals[$i] + $left + $right);
        return $vals[$i] + max($left, $right);
    };
    $dfs(0);
    return $best;
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES);
if ($lines === false || count($lines) === 0) exit(0);
$tc = intval(trim($lines[0]));
$idx = 1; $out = [];
for ($t = 0; $t < $tc; $t++) {
    $n = intval(trim($lines[$idx++]));
    $vals = array_fill(0, $n, 0);
    $ok = array_fill(0, $n, false);
    for ($i = 0; $i < $n; $i++) {
        $tok = trim($lines[$idx++]);
        if ($tok !== 'null') { $ok[$i] = true; $vals[$i] = intval($tok); }
    }
    $out[] = strval(solve($vals, $ok));
}
echo implode("\n", $out);
?>
