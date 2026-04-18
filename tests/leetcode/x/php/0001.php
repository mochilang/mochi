<?php
$data = trim(stream_get_contents(STDIN));
if ($data === '') {
    exit(0);
}
$tokens = preg_split('/\s+/', $data);
$idx = 0;
$t = intval($tokens[$idx++]);

function two_sum($nums, $target) {
    $n = count($nums);
    for ($i = 0; $i < $n; $i++) {
        for ($j = $i + 1; $j < $n; $j++) {
            if ($nums[$i] + $nums[$j] === $target) {
                return [$i, $j];
            }
        }
    }
    return [0, 0];
}

$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval($tokens[$idx++]);
    $target = intval($tokens[$idx++]);
    $nums = [];
    for ($i = 0; $i < $n; $i++) {
        $nums[] = intval($tokens[$idx++]);
    }
    [$a, $b] = two_sum($nums, $target);
    $out[] = $a . ' ' . $b;
}
echo implode(PHP_EOL, $out);
