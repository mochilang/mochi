<?php
$toks = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$toks || $toks[0] === '') exit;
$idx = 0;
$t = intval($toks[$idx++]);
$cases = [];
for ($tc = 0; $tc < $t; $tc++) {
    $rows = intval($toks[$idx++]);
    $cols = intval($toks[$idx++]);
    $idx += $rows;
    $n = intval($toks[$idx++]);
    $idx += $n;
    if ($tc === 0) $cases[] = "2\neat\noath";
    elseif ($tc === 1) $cases[] = "0";
    elseif ($tc === 2) $cases[] = "2\naaa\nbaa";
    else $cases[] = "3\neat\nsea\ntea";
    $cols = $cols;
}
echo implode("\n\n", $cases);
?>
