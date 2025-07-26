<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function split($s, $sep) {
  global $c, $rows, $cells;
  $out = [];
  $start = 0;
  $i = 0;
  $n = strlen($sep);
  while ($i <= strlen($s) - $n) {
  if (substr($s, $i, $i + $n - $i) == $sep) {
  $out = array_merge($out, [substr($s, $start, $i - $start)]);
  $i = $i + $n;
  $start = $i;
} else {
  $i = $i + 1;
}
};
  $out = array_merge($out, [substr($s, $start, strlen($s) - $start)]);
  return $out;
};
  function htmlEscape($s) {
  global $c, $rows, $cells;
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == '&') {
  $out = $out . '&amp;';
} else {
  if ($ch == '<') {
  $out = $out . '&lt;';
} else {
  if ($ch == '>') {
  $out = $out . '&gt;';
} else {
  $out = $out . $ch;
};
};
}
  $i = $i + 1;
};
  return $out;
};
  $c = 'Character,Speech\n' . 'The multitude,The messiah! Show us the messiah!\n' . 'Brians mother,<angry>Now you listen here! He\'s not the messiah; he\'s a very naughty boy! Now go away!</angry>\n' . 'The multitude,Who are you?\n' . 'Brians mother,I\'m his mother; that\'s who!\n' . 'The multitude,Behold his mother! Behold his mother!';
  $rows = [];
  foreach (explode('\n', $c) as $line) {
  $rows = array_merge($rows, [explode(',', $line)]);
}
  echo rtrim('<table>'), PHP_EOL;
  foreach ($rows as $row) {
  $cells = '';
  foreach ($row as $cell) {
  $cells = $cells . '<td>' . htmlEscape($cell) . '</td>';
};
  echo rtrim('    <tr>' . $cells . '</tr>'), PHP_EOL;
}
  echo rtrim('</table>'), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
