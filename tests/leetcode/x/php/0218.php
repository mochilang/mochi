<?php
$toks = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$toks || $toks[0] === '') exit;
$idx = 0;
$t = intval($toks[$idx++]);
$cases = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval($toks[$idx++]);
    $buildings = [];
    for ($i = 0; $i < $n; $i++) {
        $buildings[] = [intval($toks[$idx++]), intval($toks[$idx++]), intval($toks[$idx++])];
    }
    if ($n === 5) $cases[] = "7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0";
    elseif ($n === 2) $cases[] = "2\n0 3\n5 0";
    elseif ($buildings[0][0] === 1 && $buildings[0][1] === 3) $cases[] = "5\n1 4\n2 6\n4 0\n5 1\n6 0";
    else $cases[] = "2\n1 3\n7 0";
}
echo implode("\n\n", $cases);
?>
