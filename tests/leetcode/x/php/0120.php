<?php
function solve($tri) {
    $dp = $tri[count($tri) - 1];
    for ($i = count($tri) - 2; $i >= 0; $i--) {
        for ($j = 0; $j <= $i; $j++) $dp[$j] = $tri[$i][$j] + min($dp[$j], $dp[$j + 1]);
    }
    return $dp[0];
}

$toks = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$toks || $toks[0] === '') exit;
$idx = 0; $t = intval($toks[$idx++]); $out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $rows = intval($toks[$idx++]); $tri = [];
    for ($r = 1; $r <= $rows; $r++) {
        $row = [];
        for ($j = 0; $j < $r; $j++) $row[] = intval($toks[$idx++]);
        $tri[] = $row;
    }
    $out[] = strval(solve($tri));
}
echo implode("\n", $out);
?>
