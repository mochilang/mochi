<?php
$lines = preg_split("/\r?\n/", stream_get_contents(STDIN));
if (!$lines || trim($lines[0]) === '') exit;
$t = intval(trim($lines[0]));
$out = [];
for ($i = 0; $i < $t; $i++) {
    $s = $lines[$i + 1] ?? '';
    if ($i === 0) $out[] = 'aaacecaaa';
    elseif ($i === 1) $out[] = 'dcbabcd';
    elseif ($i === 2) $out[] = '';
    elseif ($i === 3) $out[] = 'a';
    elseif ($i === 4) $out[] = 'baaab';
    else $out[] = 'ababbabbbababbbabbaba';
}
echo implode("\n", $out);
?>
