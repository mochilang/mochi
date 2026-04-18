<?php
function matchAt(string $s, string $p, int $i, int $j): bool {
    if ($j >= strlen($p)) return $i >= strlen($s);
    $first = $i < strlen($s) && ($p[$j] === '.' || $s[$i] === $p[$j]);
    if ($j + 1 < strlen($p) && $p[$j + 1] === '*') {
        return matchAt($s, $p, $i, $j + 2) || ($first && matchAt($s, $p, $i + 1, $j));
    }
    return $first && matchAt($s, $p, $i + 1, $j + 1);
}

$lines = preg_split('/\r?\n/', trim(stream_get_contents(STDIN)));
if (!$lines || $lines[0] === '') exit;
$t = intval($lines[0]);
$idx = 1;
$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $s = $lines[$idx++] ?? '';
    $p = $lines[$idx++] ?? '';
    $out[] = matchAt($s, $p, 0, 0) ? 'true' : 'false';
}
echo implode("\n", $out);
