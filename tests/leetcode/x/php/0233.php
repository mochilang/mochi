<?php
function count_digit_one(int $n): int {
    $total = 0;
    for ($m = 1; $m <= $n; $m *= 10) {
        $high = intdiv($n, $m * 10);
        $cur = intdiv($n, $m) % 10;
        $low = $n % $m;
        if ($cur === 0) $total += $high * $m;
        elseif ($cur === 1) $total += $high * $m + $low + 1;
        else $total += ($high + 1) * $m;
    }
    return $total;
}

$lines = preg_split("/\r?\n/", trim(stream_get_contents(STDIN)));
if ($lines && trim($lines[0]) !== '') {
    $t = intval(trim($lines[0]));
    $out = [];
    for ($i = 0; $i < $t; $i++) {
        $out[] = strval(count_digit_one(intval(trim($lines[$i + 1] ?? '0'))));
    }
    echo implode("\n", $out);
}
?>
