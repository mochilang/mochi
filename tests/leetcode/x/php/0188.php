<?php
function solve($k, $prices) {
    $n = count($prices);
    if ($k >= intdiv($n, 2)) {
        $best = 0;
        for ($i = 1; $i < $n; $i++) if ($prices[$i] > $prices[$i - 1]) $best += $prices[$i] - $prices[$i - 1];
        return $best;
    }
    $negInf = -(1 << 60);
    $buy = array_fill(0, $k + 1, $negInf);
    $sell = array_fill(0, $k + 1, 0);
    foreach ($prices as $price) {
        for ($t = 1; $t <= $k; $t++) {
            $buy[$t] = max($buy[$t], $sell[$t - 1] - $price);
            $sell[$t] = max($sell[$t], $buy[$t] + $price);
        }
    }
    return $sell[$k];
}

$toks = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$toks || $toks[0] === '') exit;
$idx = 0;
$t = intval($toks[$idx++]);
$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $k = intval($toks[$idx++]);
    $n = intval($toks[$idx++]);
    $prices = [];
    for ($i = 0; $i < $n; $i++) $prices[] = intval($toks[$idx++]);
    $out[] = strval(solve($k, $prices));
}
echo implode("\n", $out);
?>
