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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function calc_profit($profit, $weight, $max_weight) {
  if (count($profit) != count($weight)) {
  _panic('The length of profit and weight must be same.');
}
  if ($max_weight <= 0) {
  _panic('max_weight must greater than zero.');
}
  $i = 0;
  while ($i < count($profit)) {
  if ($profit[$i] < 0) {
  _panic('Profit can not be negative.');
}
  if ($weight[$i] < 0) {
  _panic('Weight can not be negative.');
}
  $i = $i + 1;
};
  $n = count($profit);
  $used = [];
  $j = 0;
  while ($j < $n) {
  $used = _append($used, false);
  $j = $j + 1;
};
  $limit = 0;
  $gain = 0.0;
  $count = 0;
  while ($limit < $max_weight && $count < $n) {
  $maxRatio = -1.0;
  $maxIndex = -1;
  $k = 0;
  while ($k < $n) {
  if (!$used[$k]) {
  $ratio = (floatval($profit[$k])) / (floatval($weight[$k]));
  if ($ratio > $maxRatio) {
  $maxRatio = $ratio;
  $maxIndex = $k;
};
}
  $k = $k + 1;
};
  if ($maxIndex < 0) {
  break;
}
  $used[$maxIndex] = true;
  if ($max_weight - $limit >= $weight[$maxIndex]) {
  $limit = $limit + $weight[$maxIndex];
  $gain = $gain + (floatval($profit[$maxIndex]));
} else {
  $gain = $gain + (floatval(($max_weight - $limit)) / (floatval($weight[$maxIndex]))) * (floatval($profit[$maxIndex]));
  break;
}
  $count = $count + 1;
};
  return $gain;
};
  function main() {
  echo rtrim(json_encode(calc_profit([1, 2, 3], [3, 4, 5], 15), 1344)), PHP_EOL;
  echo rtrim(json_encode(calc_profit([10, 9, 8], [3, 4, 5], 25), 1344)), PHP_EOL;
  echo rtrim(json_encode(calc_profit([10, 9, 8], [3, 4, 5], 5), 1344)), PHP_EOL;
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
