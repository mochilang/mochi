<?php
function longestIncreasingPath($matrix) {
    $rows = count($matrix); $cols = count($matrix[0]);
    $memo = array_fill(0, $rows, array_fill(0, $cols, 0));
    $dirs = [[1,0],[-1,0],[0,1],[0,-1]];
    $dfs = function($r, $c) use (&$dfs, &$matrix, &$memo, $rows, $cols, $dirs) {
        if ($memo[$r][$c] != 0) return $memo[$r][$c];
        $best = 1;
        foreach ($dirs as $d) {
            $nr = $r + $d[0]; $nc = $c + $d[1];
            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols && $matrix[$nr][$nc] > $matrix[$r][$c]) {
                $best = max($best, 1 + $dfs($nr, $nc));
            }
        }
        $memo[$r][$c] = $best;
        return $best;
    };
    $ans = 0;
    for ($r = 0; $r < $rows; $r++) for ($c = 0; $c < $cols; $c++) $ans = max($ans, $dfs($r, $c));
    return $ans;
}
$data = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (count($data) && $data[0] !== '') {
    $idx = 0; $t = intval($data[$idx++]); $out = [];
    for ($tc = 0; $tc < $t; $tc++) {
        $rows = intval($data[$idx++]); $cols = intval($data[$idx++]); $m = [];
        for ($r = 0; $r < $rows; $r++) { $row = []; for ($c = 0; $c < $cols; $c++) $row[] = intval($data[$idx++]); $m[] = $row; }
        $out[] = strval(longestIncreasingPath($m));
    }
    echo implode("\n\n", $out);
}
?>
