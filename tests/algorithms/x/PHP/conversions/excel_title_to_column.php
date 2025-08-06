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
  $letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  function excel_title_to_column($title) {
  global $letters;
  $result = 0;
  $i = 0;
  while ($i < strlen($title)) {
  $ch = substr($title, $i, $i + 1 - $i);
  $value = 0;
  $idx = 0;
  $found = false;
  while ($idx < strlen($letters)) {
  if (substr($letters, $idx, $idx + 1 - $idx) == $ch) {
  $value = $idx + 1;
  $found = true;
  break;
}
  $idx = $idx + 1;
};
  if (!$found) {
  $panic('title must contain only uppercase A-Z');
}
  $result = $result * 26 + $value;
  $i = $i + 1;
};
  return $result;
};
  function main() {
  global $letters;
  echo rtrim(json_encode(excel_title_to_column('A'), 1344)), PHP_EOL;
  echo rtrim(json_encode(excel_title_to_column('B'), 1344)), PHP_EOL;
  echo rtrim(json_encode(excel_title_to_column('AB'), 1344)), PHP_EOL;
  echo rtrim(json_encode(excel_title_to_column('Z'), 1344)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
