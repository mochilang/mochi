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
  function quibble($items) {
  $n = count($items);
  if ($n == 0) {
  return '{}';
} else {
  if ($n == 1) {
  return '{' . $items[0] . '}';
} else {
  if ($n == 2) {
  return '{' . $items[0] . ' and ' . $items[1] . '}';
} else {
  $prefix = '';
  for ($i = 0; $i < $n - 1; $i++) {
  if ($i == $n - 1) {
  break;
}
  if ($i > 0) {
  $prefix = $prefix . ', ';
}
  $prefix = $prefix . $items[$i];
};
  return '{' . $prefix . ' and ' . $items[$n - 1] . '}';
};
};
}
};
  function main() {
  echo rtrim(quibble([])), PHP_EOL;
  echo rtrim(quibble(['ABC'])), PHP_EOL;
  echo rtrim(quibble(['ABC', 'DEF'])), PHP_EOL;
  echo rtrim(quibble(['ABC', 'DEF', 'G', 'H'])), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
