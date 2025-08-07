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
  function sqrt_approx($x) {
  global $A, $result;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function sign($x) {
  global $A, $result;
  if ($x >= 0.0) {
  return 1.0;
} else {
  return -1.0;
}
};
  function vector_norm($v) {
  global $A, $result;
  $sum = 0.0;
  $i = 0;
  while ($i < count($v)) {
  $sum = $sum + $v[$i] * $v[$i];
  $i = $i + 1;
};
  $n = sqrt_approx($sum);
  return $n;
};
  function identity_matrix($n) {
  global $A, $result;
  $mat = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  if ($i == $j) {
  $row = _append($row, 1.0);
} else {
  $row = _append($row, 0.0);
}
  $j = $j + 1;
};
  $mat = _append($mat, $row);
  $i = $i + 1;
};
  return $mat;
};
  function copy_matrix($a) {
  global $A, $result;
  $mat = [];
  $i = 0;
  while ($i < count($a)) {
  $row = [];
  $j = 0;
  while ($j < count($a[$i])) {
  $row = _append($row, $a[$i][$j]);
  $j = $j + 1;
};
  $mat = _append($mat, $row);
  $i = $i + 1;
};
  return $mat;
};
  function matmul($a, $b) {
  global $A, $result;
  $m = count($a);
  $n = count($a[0]);
  $p = count($b[0]);
  $res = [];
  $i = 0;
  while ($i < $m) {
  $row = [];
  $j = 0;
  while ($j < $p) {
  $sum = 0.0;
  $k = 0;
  while ($k < $n) {
  $sum = $sum + $a[$i][$k] * $b[$k][$j];
  $k = $k + 1;
};
  $row = _append($row, $sum);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function qr_decomposition($a) {
  global $A, $result;
  $m = count($a);
  $n = count($a[0]);
  $t = ($m < $n ? $m : $n);
  $q = identity_matrix($m);
  $r = copy_matrix($a);
  $k = 0;
  while ($k < $t - 1) {
  $x = [];
  $i = $k;
  while ($i < $m) {
  $x = _append($x, $r[$i][$k]);
  $i = $i + 1;
};
  $e1 = [];
  $i = 0;
  while ($i < count($x)) {
  if ($i == 0) {
  $e1 = _append($e1, 1.0);
} else {
  $e1 = _append($e1, 0.0);
}
  $i = $i + 1;
};
  $alpha = vector_norm($x);
  $s = sign($x[0]) * $alpha;
  $v = [];
  $i = 0;
  while ($i < count($x)) {
  $v = _append($v, $x[$i] + $s * $e1[$i]);
  $i = $i + 1;
};
  $vnorm = vector_norm($v);
  $i = 0;
  while ($i < count($v)) {
  $v[$i] = $v[$i] / $vnorm;
  $i = $i + 1;
};
  $size = count($v);
  $qk_small = [];
  $i = 0;
  while ($i < $size) {
  $row = [];
  $j = 0;
  while ($j < $size) {
  $delta = ($i == $j ? 1.0 : 0.0);
  $row = _append($row, $delta - 2.0 * $v[$i] * $v[$j]);
  $j = $j + 1;
};
  $qk_small = _append($qk_small, $row);
  $i = $i + 1;
};
  $qk = identity_matrix($m);
  $i = 0;
  while ($i < $size) {
  $j = 0;
  while ($j < $size) {
  $qk[$k + $i][$k + $j] = $qk_small[$i][$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
  $q = matmul($q, $qk);
  $r = matmul($qk, $r);
  $k = $k + 1;
};
  return ['q' => $q, 'r' => $r];
};
  function print_matrix($mat) {
  global $A, $result;
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
  $A = [[12.0, -51.0, 4.0], [6.0, 167.0, -68.0], [-4.0, 24.0, -41.0]];
  $result = qr_decomposition($A);
  print_matrix($result['q']);
  print_matrix($result['r']);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
