<?php
$data = trim(stream_get_contents(STDIN));
if ($data === '') exit(0);
$tokens = preg_split('/\s+/', $data);
$idx = 0;
function add_lists($a, $b) {
    $out = [];
    $i = 0; $j = 0; $carry = 0;
    while ($i < count($a) || $j < count($b) || $carry > 0) {
        $sum = $carry;
        if ($i < count($a)) $sum += $a[$i++];
        if ($j < count($b)) $sum += $b[$j++];
        $out[] = $sum % 10;
        $carry = intdiv($sum, 10);
    }
    return $out;
}
function fmt_arr($a) { return '[' . implode(',', $a) . ']'; }
$t = intval($tokens[$idx++]);
$lines = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval($tokens[$idx++]); $a = [];
    for ($i = 0; $i < $n; $i++) $a[] = intval($tokens[$idx++]);
    $m = intval($tokens[$idx++]); $b = [];
    for ($i = 0; $i < $m; $i++) $b[] = intval($tokens[$idx++]);
    $lines[] = fmt_arr(add_lists($a, $b));
}
echo implode(PHP_EOL, $lines);
