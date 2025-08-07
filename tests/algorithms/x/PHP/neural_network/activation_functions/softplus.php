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
  function ln($x) {
  if ($x <= 0.0) {
  _panic('ln domain error');
}
  $y = ($x - 1.0) / ($x + 1.0);
  $y2 = $y * $y;
  $term = $y;
  $sum = 0.0;
  $k = 0;
  while ($k < 10) {
  $denom = floatval((2 * $k + 1));
  $sum = $sum + $term / $denom;
  $term = $term * $y2;
  $k = $k + 1;
};
  return 2.0 * $sum;
};
  function mochi_exp($x) {
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 20) {
  $term = $term * $x / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function softplus($vector) {
  $result = [];
  $i = 0;
  while ($i < count($vector)) {
  $x = $vector[$i];
  $value = ln(1.0 + mochi_exp($x));
  $result = _append($result, $value);
  $i = $i + 1;
};
  return $result;
};
  function main() {
  $v1 = [2.3, 0.6, -2.0, -3.8];
  $v2 = [-9.2, -0.3, 0.45, -4.56];
  $r1 = softplus($v1);
  $r2 = softplus($v2);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($r1, 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($r2, 1344)))))), PHP_EOL;
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
