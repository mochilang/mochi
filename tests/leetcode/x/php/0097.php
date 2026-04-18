<?php
function solve($s1, $s2, $s3) {
    $m = strlen($s1); $n = strlen($s2);
    if ($m + $n != strlen($s3)) return false;
    $dp = array_fill(0, $m + 1, array_fill(0, $n + 1, false));
    $dp[0][0] = true;
    for ($i = 0; $i <= $m; $i++) {
        for ($j = 0; $j <= $n; $j++) {
            if ($i > 0 && $dp[$i-1][$j] && $s1[$i-1] === $s3[$i+$j-1]) $dp[$i][$j] = true;
            if ($j > 0 && $dp[$i][$j-1] && $s2[$j-1] === $s3[$i+$j-1]) $dp[$i][$j] = true;
        }
    }
    return $dp[$m][$n];
}

$lines = preg_split("/\\r?\\n/", stream_get_contents(STDIN));
if ($lines && trim($lines[0]) !== "") {
    $t = intval(trim($lines[0]));
    $out = [];
    for ($i = 0; $i < $t; $i++) $out[] = solve($lines[1 + 3*$i], $lines[2 + 3*$i], $lines[3 + 3*$i]) ? "true" : "false";
    echo implode("\n", $out);
}
?>
