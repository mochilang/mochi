<?php
function solve($costs) {
    if (count($costs) === 0) return 0;
    $prev = $costs[0];
    for ($r = 1; $r < count($costs); $r++) {
        $min1 = PHP_INT_MAX; $min2 = PHP_INT_MAX; $idx1 = -1;
        for ($i = 0; $i < count($prev); $i++) {
            $v = $prev[$i];
            if ($v < $min1) {
                $min2 = $min1;
                $min1 = $v;
                $idx1 = $i;
            } elseif ($v < $min2) {
                $min2 = $v;
            }
        }
        $cur = [];
        for ($i = 0; $i < count($prev); $i++) {
            $cur[] = $costs[$r][$i] + ($i === $idx1 ? $min2 : $min1);
        }
        $prev = $cur;
    }
    return min($prev);
}

$toks = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if ($toks && $toks[0] !== '') {
    $idx = 0;
    $t = intval($toks[$idx++]);
    $out = [];
    for ($tc = 0; $tc < $t; $tc++) {
        $n = intval($toks[$idx++]);
        $k = intval($toks[$idx++]);
        $costs = [];
        for ($i = 0; $i < $n; $i++) {
            $row = [];
            for ($j = 0; $j < $k; $j++) $row[] = intval($toks[$idx++]);
            $costs[] = $row;
        }
        $out[] = strval(solve($costs));
    }
    echo implode("\n", $out);
}
?>
