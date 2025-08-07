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
  function floyd($n) {
  $result = '';
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - $i - 1) {
  $result = $result . ' ';
  $j = $j + 1;
};
  $k = 0;
  while ($k < $i + 1) {
  $result = $result . '* ';
  $k = $k + 1;
};
  $result = $result . '
';
  $i = $i + 1;
};
  return $result;
};
  function reverse_floyd($n) {
  $result = '';
  $i = $n;
  while ($i > 0) {
  $j = $i;
  while ($j > 0) {
  $result = $result . '* ';
  $j = $j - 1;
};
  $result = $result . '
';
  $k = $n - $i + 1;
  while ($k > 0) {
  $result = $result . ' ';
  $k = $k - 1;
};
  $i = $i - 1;
};
  return $result;
};
  function pretty_print($n) {
  if ($n <= 0) {
  return '       ...       ....        nothing printing :(';
}
  $upper_half = floyd($n);
  $lower_half = reverse_floyd($n);
  return $upper_half . $lower_half;
};
  function main() {
  echo rtrim(pretty_print(3)), PHP_EOL;
  echo rtrim(pretty_print(0)), PHP_EOL;
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
