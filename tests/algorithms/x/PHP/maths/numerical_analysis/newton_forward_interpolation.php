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
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function ucal($u, $p) {
  global $x_points, $y_points;
  $temp = $u;
  $i = 1;
  while ($i < $p) {
  $temp = $temp * ($u - (floatval($i)));
  $i = $i + 1;
};
  return $temp;
};
  function factorial($n) {
  global $x_points, $y_points;
  $result = 1.0;
  $i = 2;
  while ($i <= $n) {
  $result = $result * (floatval($i));
  $i = $i + 1;
};
  return $result;
};
  function newton_forward_interpolation($x, $y0, $value) {
  global $x_points, $y_points;
  $n = count($x);
  $y = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, 0.0);
  $j = $j + 1;
};
  $y = _append($y, $row);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  $y[$i][0] = $y0[$i];
  $i = $i + 1;
};
  $i1 = 1;
  while ($i1 < $n) {
  $j1 = 0;
  while ($j1 < $n - $i1) {
  $y[$j1][$i1] = $y[$j1 + 1][$i1 - 1] - $y[$j1][$i1 - 1];
  $j1 = $j1 + 1;
};
  $i1 = $i1 + 1;
};
  $u = ($value - $x[0]) / ($x[1] - $x[0]);
  $sum = $y[0][0];
  $k = 1;
  while ($k < $n) {
  $sum = $sum + (ucal($u, $k) * $y[0][$k]) / factorial($k);
  $k = $k + 1;
};
  return $sum;
};
  $x_points = [0.0, 1.0, 2.0, 3.0];
  $y_points = [0.0, 1.0, 8.0, 27.0];
  echo rtrim(_str(newton_forward_interpolation($x_points, $y_points, 1.5))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
