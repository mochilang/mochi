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
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_exp($x) {
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 20) {
  $term = $term * $x / floatval($n);
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function soboleva_modified_hyperbolic_tangent($vector, $a_value, $b_value, $c_value, $d_value) {
  $result = [];
  $i = 0;
  while ($i < count($vector)) {
  $x = $vector[$i];
  $numerator = mochi_exp($a_value * $x) - mochi_exp(-$b_value * $x);
  $denominator = mochi_exp($c_value * $x) + mochi_exp(-$d_value * $x);
  $result = _append($result, $numerator / $denominator);
  $i = $i + 1;
};
  return $result;
};
  function main() {
  $vector = [5.4, -2.4, 6.3, -5.23, 3.27, 0.56];
  $res = soboleva_modified_hyperbolic_tangent($vector, 0.2, 0.4, 0.6, 0.8);
  echo str_replace('    ', '  ', json_encode($res, 128)), PHP_EOL;
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
