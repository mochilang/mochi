<?php
function candy($ratings) {
    $n = count($ratings);
    $candies = array_fill(0, $n, 1);
    for ($i = 1; $i < $n; $i++) {
        if ($ratings[$i] > $ratings[$i - 1]) $candies[$i] = $candies[$i - 1] + 1;
    }
    for ($i = $n - 2; $i >= 0; $i--) {
        if ($ratings[$i] > $ratings[$i + 1]) $candies[$i] = max($candies[$i], $candies[$i + 1] + 1);
    }
    return array_sum($candies);
}

$lines = preg_split('/\r?\n/', trim(stream_get_contents(STDIN)));
if (!$lines || $lines[0] === '') exit(0);
$tc = intval($lines[0]);
$idx = 1;
$out = [];
for ($t = 0; $t < $tc; $t++) {
    $n = intval($lines[$idx++]);
    $ratings = [];
    for ($i = 0; $i < $n; $i++) $ratings[] = intval($lines[$idx++]);
    $out[] = strval(candy($ratings));
}
echo implode("\n\n", $out);
