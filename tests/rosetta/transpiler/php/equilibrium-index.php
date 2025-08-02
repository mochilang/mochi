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
  $seed = fmod(_now(), 2147483647);
  function randN($n) {
  global $seed;
  $seed = ($seed * 1664525 + 1013904223) % 2147483647;
  return $seed % $n;
};
  function eqIndices($xs) {
  global $seed;
  $r = 0;
  $i = 0;
  while ($i < count($xs)) {
  $r = $r + $xs[$i];
  $i = $i + 1;
};
  $l = 0;
  $eq = [];
  $i = 0;
  while ($i < count($xs)) {
  $r = $r - $xs[$i];
  if ($l == $r) {
  $eq = array_merge($eq, [$i]);
}
  $l = $l + $xs[$i];
  $i = $i + 1;
};
  return $eq;
};
  function main() {
  global $seed;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(eqIndices([-7, 1, 5, 2, -4, 3, 0]), 1344))))))), PHP_EOL;
  $verylong = [];
  $i = 0;
  while ($i < 10000) {
  $seed = ($seed * 1664525 + 1013904223) % 2147483647;
  $verylong = array_merge($verylong, [$seed % 1001 - 500]);
  $i = $i + 1;
};
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(eqIndices($verylong), 1344))))))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
