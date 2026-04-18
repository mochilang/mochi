<?php
$toks = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$toks || $toks[0] === '') exit;
$idx = 0;
$t = intval($toks[$idx++]);

function solve($a) {
    $best = 0;
    for ($i = 0; $i < count($a); $i++) {
        $mn = $a[$i];
        for ($j = $i; $j < count($a); $j++) {
            if ($a[$j] < $mn) $mn = $a[$j];
            $area = $mn * ($j - $i + 1);
            if ($area > $best) $best = $area;
        }
    }
    return $best;
}

$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval($toks[$idx++]);
    $a = [];
    for ($i = 0; $i < $n; $i++) $a[] = intval($toks[$idx++]);
    $out[] = strval(solve($a));
}
echo implode("\n", $out);
?>
