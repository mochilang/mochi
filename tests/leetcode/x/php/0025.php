<?php
$lines = preg_split('/\r?\n/', trim(stream_get_contents(STDIN)));
if (!$lines || $lines[0] === '') exit;
$idx = 0; $t = intval(trim($lines[$idx++])); $out = [];
for ($tc = 0; $tc < $t; $tc++) {
  $n = $idx < count($lines) ? intval(trim($lines[$idx++])) : 0;
  $arr = [];
  for ($i = 0; $i < $n; $i++) $arr[] = $idx < count($lines) ? intval(trim($lines[$idx++])) : 0;
  $k = $idx < count($lines) ? intval(trim($lines[$idx++])) : 1;
  for ($i = 0; $i + $k <= count($arr); $i += $k) {
    for ($l = $i, $r = $i + $k - 1; $l < $r; $l++, $r--) { $tmp = $arr[$l]; $arr[$l] = $arr[$r]; $arr[$r] = $tmp; }
  }
  $out[] = '[' . implode(',', $arr) . ']';
}
echo implode("\n", $out);
