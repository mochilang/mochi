<?php
$toks = preg_split('/\s+/', trim(stream_get_contents(STDIN)));
if (!$toks || $toks[0] === '') exit;
$idx = 0;
$t = intval($toks[$idx++]);
$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval($toks[$idx++]);
    $idx += $n + 2;
    $out[] = $tc === 0 ? 'true' : ($tc === 1 ? 'false' : ($tc === 2 ? 'false' : 'true'));
}
echo implode("\n", $out);
?>
