<?php
$lines = array_values(array_filter(array_map('trim', file('php://stdin')), 'strlen'));
if (!$lines) exit(0);
$t = intval($lines[0]);
$idx = 1;
$blocks = [];
for ($tc = 0; $tc < $t; $tc++) {
    [$r, $c] = array_map('intval', preg_split('/\s+/', $lines[$idx++]));
    $image = [];
    for ($i = 0; $i < $r; $i++) $image[] = $lines[$idx++];
    [$x, $y] = array_map('intval', preg_split('/\s+/', $lines[$idx++]));
    $top = $r;
    $bottom = -1;
    $left = strlen($image[0]);
    $right = -1;
    for ($i = 0; $i < $r; $i++) {
        for ($j = 0; $j < strlen($image[$i]); $j++) {
            if ($image[$i][$j] === '1') {
                $top = min($top, $i);
                $bottom = max($bottom, $i);
                $left = min($left, $j);
                $right = max($right, $j);
            }
        }
    }
    $blocks[] = strval(($bottom - $top + 1) * ($right - $left + 1));
}
echo implode("\n\n", $blocks);
