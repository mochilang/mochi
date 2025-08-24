<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function zeros($n) {
  global $arr;
  $res = [];
  $i = 0;
  while ($i < $n) {
  $res = _append($res, 0);
  $i = $i + 1;
};
  return $res;
};
  function update(&$arr, $idx, $value) {
  $arr[$idx] = $value;
};
  function query($arr, $left, $right) {
  $result = 0;
  $i = $left;
  while ($i < $right) {
  if ($arr[$i] > $result) {
  $result = $arr[$i];
}
  $i = $i + 1;
};
  return $result;
};
  $arr = [0, 0, 0, 0, 0];
  echo rtrim(json_encode(query($arr, 0, 5), 1344)), PHP_EOL;
  update($arr, 4, 100);
  echo rtrim(json_encode(query($arr, 0, 5), 1344)), PHP_EOL;
  update($arr, 4, 0);
  update($arr, 2, 20);
  echo rtrim(json_encode(query($arr, 0, 5), 1344)), PHP_EOL;
  update($arr, 4, 10);
  echo rtrim(json_encode(query($arr, 2, 5), 1344)), PHP_EOL;
  echo rtrim(json_encode(query($arr, 1, 5), 1344)), PHP_EOL;
  update($arr, 2, 0);
  echo rtrim(json_encode(query($arr, 0, 5), 1344)), PHP_EOL;
  $arr = zeros(10000);
  update($arr, 255, 30);
  echo rtrim(json_encode(query($arr, 0, 10000), 1344)), PHP_EOL;
  $arr = zeros(6);
  update($arr, 5, 1);
  echo rtrim(json_encode(query($arr, 5, 6), 1344)), PHP_EOL;
  $arr = zeros(6);
  update($arr, 0, 1000);
  echo rtrim(json_encode(query($arr, 0, 1), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
