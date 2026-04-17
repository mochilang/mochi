<?php
$data = trim(stream_get_contents(STDIN));
if ($data === '') exit(0);
$tokens = preg_split('/\s+/', $data);
$values = ['I' => 1, 'V' => 5, 'X' => 10, 'L' => 50, 'C' => 100, 'D' => 500, 'M' => 1000];

function roman_to_int($s, $values) {
    $total = 0;
    $n = strlen($s);
    for ($i = 0; $i < $n; $i++) {
        $cur = $values[$s[$i]];
        $next = $i + 1 < $n ? $values[$s[$i + 1]] : 0;
        $total += $cur < $next ? -$cur : $cur;
    }
    return $total;
}

$t = intval($tokens[0]);
for ($i = 0; $i < $t; $i++) {
    echo roman_to_int($tokens[$i + 1], $values);
    if ($i + 1 < $t) echo PHP_EOL;
}
