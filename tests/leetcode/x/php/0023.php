<?php
$lines = preg_split('/?
/', trim(stream_get_contents(STDIN)));
if (!$lines || $lines[0] === '') exit;
$idx = 0; $t = intval(trim($lines[$idx++])); $out = [];
for ($tc = 0; $tc < $t; $tc++) {
    $k = $idx < count($lines) ? intval(trim($lines[$idx++])) : 0; $vals = [];
    for ($i = 0; $i < $k; $i++) {
        $n = $idx < count($lines) ? intval(trim($lines[$idx++])) : 0;
        for ($j = 0; $j < $n; $j++) $vals[] = $idx < count($lines) ? intval(trim($lines[$idx++])) : 0;
    }
    sort($vals);
    $out[] = '[' . implode(',', $vals) . ']';
}
echo implode("
", $out);
