<?php
function solveNQueens($n) {
    $cols = array_fill(0, $n, false);
    $d1 = array_fill(0, 2 * $n, false);
    $d2 = array_fill(0, 2 * $n, false);
    $board = array_fill(0, $n, str_repeat('.', $n));
    $res = [];
    $dfs = function($r) use ($n, &$cols, &$d1, &$d2, &$board, &$res, &$dfs) {
        if ($r == $n) { $res[] = $board; return; }
        for ($c = 0; $c < $n; $c++) {
            $a = $r + $c; $b = $r - $c + $n - 1;
            if ($cols[$c] || $d1[$a] || $d2[$b]) continue;
            $cols[$c] = $d1[$a] = $d2[$b] = true;
            $row = $board[$r]; $row[$c] = 'Q'; $board[$r] = $row;
            $dfs($r + 1);
            $row = $board[$r]; $row[$c] = '.'; $board[$r] = $row;
            $cols[$c] = $d1[$a] = $d2[$b] = false;
        }
    };
    $dfs(0);
    return $res;
}
$lines = file('php://stdin', FILE_IGNORE_NEW_LINES); if (!$lines) exit;
$idx = 0; $t = intval(trim($lines[$idx++])); $out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval(trim($lines[$idx++]));
    $sols = solveNQueens($n);
    $out[] = strval(count($sols));
    foreach ($sols as $si => $sol) { foreach ($sol as $row) $out[] = $row; if ($si + 1 < count($sols)) $out[] = '-'; }
    if ($tc + 1 < $t) $out[] = '=';
}
echo implode("\n", $out);
?>
