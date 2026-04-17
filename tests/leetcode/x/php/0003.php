<?php
$lines = preg_split('/?
/', stream_get_contents(STDIN));
if ($lines === false || count($lines) === 0 || trim($lines[0]) === '') exit(0);
function longest($s) {
    $last = []; $left = 0; $best = 0; $n = strlen($s);
    for ($right = 0; $right < $n; $right++) {
        $ch = $s[$right];
        if (isset($last[$ch]) && $last[$ch] >= $left) $left = $last[$ch] + 1;
        $last[$ch] = $right;
        $best = max($best, $right - $left + 1);
    }
    return $best;
}
$t = intval(trim($lines[0]));
$out = [];
for ($i = 0; $i < $t; $i++) $out[] = strval(longest($lines[$i + 1] ?? ''));
echo implode(PHP_EOL, $out);
