<?php
$data = trim(stream_get_contents(STDIN));
if ($data === '') exit(0);
$tokens = preg_split('/\s+/', $data);
$idx = 0; $t = intval($tokens[$idx++]);

function lcp($strs) {
    $prefix = $strs[0];
    while (true) {
        $ok = true;
        foreach ($strs as $s) {
            if (!str_starts_with($s, $prefix)) { $ok = false; break; }
        }
        if ($ok) return $prefix;
        $prefix = substr($prefix, 0, -1);
    }
}

$out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $n = intval($tokens[$idx++]);
    $strs = array_slice($tokens, $idx, $n);
    $idx += $n;
    $out[] = '"' . lcp($strs) . '"';
}
echo implode(PHP_EOL, $out);
