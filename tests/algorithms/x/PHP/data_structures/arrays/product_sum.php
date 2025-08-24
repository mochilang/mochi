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
function _exists($v) {
    if (is_array($v)) return count($v) > 0;
    if (is_string($v)) return strlen($v) > 0;
    if (is_object($v) && property_exists($v, 'Items')) return count($v->Items) > 0;
    return false;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function product_sum($arr, $depth) {
  global $example;
  $total = 0;
  $i = 0;
  while ($i < count($arr)) {
  $el = $arr[$i];
  if (_exists($el)) {
  $total = $total + product_sum($el, $depth + 1);
} else {
  $total = $total + intval($el);
}
  $i = $i + 1;
};
  return $total * $depth;
};
  function product_sum_array($array) {
  global $example;
  $res = product_sum($array, 1);
  return $res;
};
  $example = [5, 2, [-7, 1], 3, [6, [-13, 8], 4]];
  echo rtrim(json_encode(product_sum_array($example), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
