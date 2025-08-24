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
  function empty_list() {
  return ['data' => []];
};
  function push($lst, $value) {
  $res = [$value];
  $i = 0;
  while ($i < count($lst['data'])) {
  $res = _append($res, $lst['data'][$i]);
  $i = $i + 1;
};
  return ['data' => $res];
};
  function middle_element($lst) {
  $n = count($lst['data']);
  if ($n == 0) {
  echo rtrim('No element found.'), PHP_EOL;
  return 0;
}
  $slow = 0;
  $fast = 0;
  while ($fast + 1 < $n) {
  $fast = $fast + 2;
  $slow = $slow + 1;
};
  return $lst['data'][$slow];
};
  function main() {
  $lst = empty_list();
  middle_element($lst);
  $lst = push($lst, 5);
  echo rtrim(json_encode(5, 1344)), PHP_EOL;
  $lst = push($lst, 6);
  echo rtrim(json_encode(6, 1344)), PHP_EOL;
  $lst = push($lst, 8);
  echo rtrim(json_encode(8, 1344)), PHP_EOL;
  $lst = push($lst, 8);
  echo rtrim(json_encode(8, 1344)), PHP_EOL;
  $lst = push($lst, 10);
  echo rtrim(json_encode(10, 1344)), PHP_EOL;
  $lst = push($lst, 12);
  echo rtrim(json_encode(12, 1344)), PHP_EOL;
  $lst = push($lst, 17);
  echo rtrim(json_encode(17, 1344)), PHP_EOL;
  $lst = push($lst, 7);
  echo rtrim(json_encode(7, 1344)), PHP_EOL;
  $lst = push($lst, 3);
  echo rtrim(json_encode(3, 1344)), PHP_EOL;
  $lst = push($lst, 20);
  echo rtrim(json_encode(20, 1344)), PHP_EOL;
  $lst = push($lst, -20);
  echo rtrim(json_encode(-20, 1344)), PHP_EOL;
  echo rtrim(json_encode(middle_element($lst), 1344)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
