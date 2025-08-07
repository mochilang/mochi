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
  function neville_interpolate($x_points, $y_points, $x0) {
  $n = count($x_points);
  $q = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, 0.0);
  $j = $j + 1;
};
  $q = _append($q, $row);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  $q[$i][1] = $y_points[$i];
  $i = $i + 1;
};
  $col = 2;
  while ($col < $n) {
  $row_idx = $col;
  while ($row_idx < $n) {
  $q[$row_idx][$col] = (($x0 - $x_points[$row_idx - $col + 1]) * $q[$row_idx][$col - 1] - ($x0 - $x_points[$row_idx]) * $q[$row_idx - 1][$col - 1]) / ($x_points[$row_idx] - $x_points[$row_idx - $col + 1]);
  $row_idx = $row_idx + 1;
};
  $col = $col + 1;
};
  return ['value' => $q[$n - 1][$n - 1], 'table' => $q];
};
  function test_neville() {
  $xs = [1.0, 2.0, 3.0, 4.0, 6.0];
  $ys = [6.0, 7.0, 8.0, 9.0, 11.0];
  $r1 = neville_interpolate($xs, $ys, 5.0);
  if ($r1['value'] != 10.0) {
  $panic('neville_interpolate at 5 failed');
}
  $r2 = neville_interpolate($xs, $ys, 99.0);
  if ($r2['value'] != 104.0) {
  $panic('neville_interpolate at 99 failed');
}
};
  function main() {
  test_neville();
  $xs = [1.0, 2.0, 3.0, 4.0, 6.0];
  $ys = [6.0, 7.0, 8.0, 9.0, 11.0];
  $r = neville_interpolate($xs, $ys, 5.0);
  echo rtrim(json_encode($r['value'], 1344)), PHP_EOL;
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
