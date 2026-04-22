<?php
function shortestDistance(array $grid): int {
    $rows = count($grid);
    $cols = count($grid[0]);
    $dist = array_fill(0, $rows, array_fill(0, $cols, 0));
    $reach = array_fill(0, $rows, array_fill(0, $cols, 0));
    $buildings = 0;
    for ($sr = 0; $sr < $rows; $sr++) {
        for ($sc = 0; $sc < $cols; $sc++) {
            if ($grid[$sr][$sc] !== 1) continue;
            $buildings++;
            $seen = array_fill(0, $rows, array_fill(0, $cols, false));
            $q = [[$sr, $sc, 0]];
            $seen[$sr][$sc] = true;
            for ($head = 0; $head < count($q); $head++) {
                [$r, $c, $d] = $q[$head];
                foreach ([[1,0],[-1,0],[0,1],[0,-1]] as [$dr, $dc]) {
                    $nr = $r + $dr;
                    $nc = $c + $dc;
                    if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols && !$seen[$nr][$nc]) {
                        $seen[$nr][$nc] = true;
                        if ($grid[$nr][$nc] === 0) {
                            $dist[$nr][$nc] += $d + 1;
                            $reach[$nr][$nc] += 1;
                            $q[] = [$nr, $nc, $d + 1];
                        }
                    }
                }
            }
        }
    }
    $ans = null;
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if ($grid[$r][$c] === 0 && $reach[$r][$c] === $buildings) {
                $ans = $ans === null ? $dist[$r][$c] : min($ans, $dist[$r][$c]);
            }
        }
    }
    return $ans === null ? -1 : $ans;
}

$data = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$data || $data[0] === '') exit(0);
$pos = 0;
$t = intval($data[$pos++]);
$blocks = [];
for ($tc = 0; $tc < $t; $tc++) {
    $rows = intval($data[$pos++]);
    $cols = intval($data[$pos++]);
    $grid = [];
    for ($i = 0; $i < $rows; $i++) {
        $row = [];
        for ($j = 0; $j < $cols; $j++) $row[] = intval($data[$pos++]);
        $grid[] = $row;
    }
    $blocks[] = strval(shortestDistance($grid));
}
echo implode("\n\n", $blocks);
