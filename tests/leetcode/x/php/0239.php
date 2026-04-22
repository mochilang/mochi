<?php
function solve($nums, $k) {
    $dq = [];
    $ans = [];
    foreach ($nums as $i => $x) {
        if (count($dq) > 0 && $dq[0] <= $i - $k) array_shift($dq);
        while (count($dq) > 0 && $nums[$dq[count($dq) - 1]] <= $x) array_pop($dq);
        $dq[] = $i;
        if ($i >= $k - 1) $ans[] = $nums[$dq[0]];
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
        $nums = [];
        for ($i = 0; $i < $n; $i++) $nums[] = intval($toks[$idx++]);
        $k = intval($toks[$idx++]);
        $ans = solve($nums, $k);
        $blocks[] = implode("\n", array_merge([strval(count($ans))], array_map('strval', $ans)));
    }
    echo implode("\n\n", $blocks);
}
?>
