<?php
function solve($dungeon) {
    $cols = count($dungeon[0]);
    $inf = 1 << 60;
    $dp = array_fill(0, $cols + 1, $inf);
    $dp[$cols - 1] = 1;
    for ($i = count($dungeon) - 1; $i >= 0; $i--) {
        for ($j = $cols - 1; $j >= 0; $j--) {
            $need = min($dp[$j], $dp[$j + 1]) - $dungeon[$i][$j];
            $dp[$j] = $need <= 1 ? 1 : $need;
        }
    }
    return $dp[0];
}

$toks = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$toks || $toks[0] === '') exit;
$idx = 0;
$t = intval($toks[$idx++]);
$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $rows = intval($toks[$idx++]);
    $cols = intval($toks[$idx++]);
    $dungeon = [];
    for ($i = 0; $i < $rows; $i++) {
        $row = [];
        for ($j = 0; $j < $cols; $j++) $row[] = intval($toks[$idx++]);
        $dungeon[] = $row;
    }
    $out[] = strval(solve($dungeon));
}
echo implode("\n", $out);
?>
