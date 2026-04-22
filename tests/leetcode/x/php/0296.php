<?php
$data = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$data || $data[0] === '') {
    exit(0);
}
$idx = 0;
$t = intval($data[$idx++]);
$blocks = [];
for ($tc = 0; $tc < $t; $tc++) {
    $r = intval($data[$idx++]);
    $c = intval($data[$idx++]);
    $grid = [];
    for ($i = 0; $i < $r; $i++) {
        $row = [];
        for ($j = 0; $j < $c; $j++) {
            $row[] = intval($data[$idx++]);
        }
        $grid[] = $row;
    }
    $rows = [];
    $cols = [];
    for ($i = 0; $i < $r; $i++) {
        for ($j = 0; $j < $c; $j++) {
            if ($grid[$i][$j] === 1) $rows[] = $i;
        }
    }
    for ($j = 0; $j < $c; $j++) {
        for ($i = 0; $i < $r; $i++) {
            if ($grid[$i][$j] === 1) $cols[] = $j;
        }
    }
    $mr = $rows[intdiv(count($rows), 2)];
    $mc = $cols[intdiv(count($cols), 2)];
    $ans = 0;
    foreach ($rows as $x) $ans += abs($x - $mr);
    foreach ($cols as $x) $ans += abs($x - $mc);
    $blocks[] = strval($ans);
}
echo implode("\n\n", $blocks);
