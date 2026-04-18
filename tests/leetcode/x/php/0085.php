<?php
$lines = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$lines || $lines[0] === '') exit;
$idx = 0;
$t = intval($lines[$idx++]);

function hist($h) {
    $best = 0;
    for ($i = 0; $i < count($h); $i++) {
        $mn = $h[$i];
        for ($j = $i; $j < count($h); $j++) {
            if ($h[$j] < $mn) $mn = $h[$j];
            $area = $mn * ($j - $i + 1);
            if ($area > $best) $best = $area;
        }
    }
    return $best;
}

$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $rows = intval($lines[$idx++]);
    $cols = intval($lines[$idx++]);
    $h = array_fill(0, $cols, 0);
    $best = 0;
    for ($r = 0; $r < $rows; $r++) {
        $s = $lines[$idx++];
        for ($c = 0; $c < $cols; $c++) $h[$c] = $s[$c] === '1' ? $h[$c] + 1 : 0;
        $best = max($best, hist($h));
    }
    $out[] = strval($best);
}
echo implode("\n", $out);
?>
