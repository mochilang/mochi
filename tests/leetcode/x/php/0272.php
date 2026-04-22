<?php
function solve($values, $target, $k) {
    $right = 0;
    while ($right < count($values) && $values[$right] < $target) $right++;
    $left = $right - 1;
    $ans = [];
    while (count($ans) < $k) {
        if ($left < 0) $ans[] = $values[$right++];
        elseif ($right >= count($values)) $ans[] = $values[$left--];
        elseif (abs($values[$left] - $target) <= abs($values[$right] - $target)) $ans[] = $values[$left--];
        else $ans[] = $values[$right++];
    }
    return $ans;
}

$toks = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if ($toks && $toks[0] !== '') {
    $idx = 0;
    $t = intval($toks[$idx++]);
    $blocks = [];
    for ($tc = 0; $tc < $t; $tc++) {
        $n = intval($toks[$idx++]);
        $values = [];
        for ($i = 0; $i < $n; $i++) $values[] = intval($toks[$idx++]);
        $target = floatval($toks[$idx++]);
        $k = intval($toks[$idx++]);
        $ans = solve($values, $target, $k);
        $blocks[] = implode("\n", array_merge([strval(count($ans))], array_map('strval', $ans)));
    }
    echo implode("\n\n", $blocks);
}
?>
