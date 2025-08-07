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
  function zeros($n) {
  global $A, $b, $x;
  $res = [];
  $i = 0;
  while ($i < $n) {
  $res = _append($res, 0.0);
  $i = $i + 1;
};
  return $res;
};
  function dot($a, $b) {
  global $A, $x;
  $sum = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $sum = $sum + $a[$i] * $b[$i];
  $i = $i + 1;
};
  return $sum;
};
  function mat_vec_mul($m, $v) {
  global $A, $b, $x;
  $res = [];
  $i = 0;
  while ($i < count($m)) {
  $s = 0.0;
  $j = 0;
  while ($j < count($m[$i])) {
  $s = $s + $m[$i][$j] * $v[$j];
  $j = $j + 1;
};
  $res = _append($res, $s);
  $i = $i + 1;
};
  return $res;
};
  function vec_add($a, $b) {
  global $A, $x;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = _append($res, $a[$i] + $b[$i]);
  $i = $i + 1;
};
  return $res;
};
  function vec_sub($a, $b) {
  global $A, $x;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = _append($res, $a[$i] - $b[$i]);
  $i = $i + 1;
};
  return $res;
};
  function scalar_mul($s, $v) {
  global $A, $b, $x;
  $res = [];
  $i = 0;
  while ($i < count($v)) {
  $res = _append($res, $s * $v[$i]);
  $i = $i + 1;
};
  return $res;
};
  function sqrtApprox($x) {
  global $A, $b;
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
  function norm($v) {
  global $A, $b, $x;
  return sqrtApprox(dot($v, $v));
};
  function conjugate_gradient($A, $b, $max_iterations, $tol) {
  $n = count($b);
  $x = zeros($n);
  $r = vec_sub($b, mat_vec_mul($A, $x));
  $p = $r;
  $rs_old = dot($r, $r);
  $i = 0;
  while ($i < $max_iterations) {
  $Ap = mat_vec_mul($A, $p);
  $alpha = $rs_old / dot($p, $Ap);
  $x = vec_add($x, scalar_mul($alpha, $p));
  $r = vec_sub($r, scalar_mul($alpha, $Ap));
  $rs_new = dot($r, $r);
  if (sqrtApprox($rs_new) < $tol) {
  break;
}
  $beta = $rs_new / $rs_old;
  $p = vec_add($r, scalar_mul($beta, $p));
  $rs_old = $rs_new;
  $i = $i + 1;
};
  return $x;
};
  $A = [[8.73256573, -5.02034289, -2.68709226], [-5.02034289, 3.78188322, 0.91980451], [-2.68709226, 0.91980451, 1.94746467]];
  $b = [-5.80872761, 3.23807431, 1.95381422];
  $x = conjugate_gradient($A, $b, 1000, 0.00000001);
  echo rtrim(_str($x[0])), PHP_EOL;
  echo rtrim(_str($x[1])), PHP_EOL;
  echo rtrim(_str($x[2])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
