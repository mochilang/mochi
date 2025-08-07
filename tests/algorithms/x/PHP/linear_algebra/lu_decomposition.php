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
  function lu_decomposition($mat) {
  global $matrix, $result;
  $n = count($mat);
  if ($n == 0) {
  return ['lower' => [], 'upper' => []];
}
  $m = count($mat[0]);
  if ($n != $m) {
  $panic('Matrix must be square');
}
  $lower = [];
  $upper = [];
  $i = 0;
  while ($i < $n) {
  $lrow = [];
  $urow = [];
  $j = 0;
  while ($j < $n) {
  $lrow = _append($lrow, 0.0);
  $urow = _append($urow, 0.0);
  $j = $j + 1;
};
  $lower = _append($lower, $lrow);
  $upper = _append($upper, $urow);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  $j1 = 0;
  while ($j1 < $i) {
  $total = 0.0;
  $k = 0;
  while ($k < $i) {
  $total = $total + $lower[$i][$k] * $upper[$k][$j1];
  $k = $k + 1;
};
  if ($upper[$j1][$j1] == 0.0) {
  $panic('No LU decomposition exists');
}
  $lower[$i][$j1] = ($mat[$i][$j1] - $total) / $upper[$j1][$j1];
  $j1 = $j1 + 1;
};
  $lower[$i][$i] = 1.0;
  $j2 = $i;
  while ($j2 < $n) {
  $total2 = 0.0;
  $k2 = 0;
  while ($k2 < $i) {
  $total2 = $total2 + $lower[$i][$k2] * $upper[$k2][$j2];
  $k2 = $k2 + 1;
};
  $upper[$i][$j2] = $mat[$i][$j2] - $total2;
  $j2 = $j2 + 1;
};
  $i = $i + 1;
};
  return ['lower' => $lower, 'upper' => $upper];
};
  function print_matrix($mat) {
  global $matrix, $result;
  $i = 0;
  while ($i < count($mat)) {
  $line = '';
  $j = 0;
  while ($j < count($mat[$i])) {
  $line = $line . _str($mat[$i][$j]);
  if ($j + 1 < count($mat[$i])) {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  $matrix = [[2.0, -2.0, 1.0], [0.0, 1.0, 2.0], [5.0, 3.0, 1.0]];
  $result = lu_decomposition($matrix);
  print_matrix($result['lower']);
  print_matrix($result['upper']);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
