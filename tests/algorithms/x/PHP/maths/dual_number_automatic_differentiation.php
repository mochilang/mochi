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
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function make_dual($real, $rank) {
  $ds = [];
  $i = 0;
  while ($i < $rank) {
  $ds = _append($ds, 1.0);
  $i = $i + 1;
};
  return ['real' => $real, 'duals' => $ds];
};
  function dual_from_list($real, $ds) {
  return ['real' => $real, 'duals' => $ds];
};
  function dual_add($a, $b) {
  $s_dual = [];
  $i = 0;
  while ($i < _len($a['duals'])) {
  $s_dual = _append($s_dual, $a['duals'][$i]);
  $i = $i + 1;
};
  $o_dual = [];
  $j = 0;
  while ($j < _len($b['duals'])) {
  $o_dual = _append($o_dual, $b['duals'][$j]);
  $j = $j + 1;
};
  if (count($s_dual) > count($o_dual)) {
  $diff = count($s_dual) - count($o_dual);
  $k = 0;
  while ($k < $diff) {
  $o_dual = _append($o_dual, 1.0);
  $k = $k + 1;
};
} else {
  if (count($s_dual) < count($o_dual)) {
  $diff2 = count($o_dual) - count($s_dual);
  $k2 = 0;
  while ($k2 < $diff2) {
  $s_dual = _append($s_dual, 1.0);
  $k2 = $k2 + 1;
};
};
}
  $new_duals = [];
  $idx = 0;
  while ($idx < count($s_dual)) {
  $new_duals = _append($new_duals, $s_dual[$idx] + $o_dual[$idx]);
  $idx = $idx + 1;
};
  return ['real' => $a['real'] + $b['real'], 'duals' => $new_duals];
};
  function dual_add_real($a, $b) {
  $ds = [];
  $i = 0;
  while ($i < _len($a['duals'])) {
  $ds = _append($ds, $a['duals'][$i]);
  $i = $i + 1;
};
  return ['real' => $a['real'] + $b, 'duals' => $ds];
};
  function dual_mul($a, $b) {
  $new_len = _len($a['duals']) + _len($b['duals']) + 1;
  $new_duals = [];
  $idx = 0;
  while ($idx < $new_len) {
  $new_duals = _append($new_duals, 0.0);
  $idx = $idx + 1;
};
  $i = 0;
  while ($i < _len($a['duals'])) {
  $j = 0;
  while ($j < _len($b['duals'])) {
  $pos = $i + $j + 1;
  $val = $new_duals[$pos] + $a['duals'][$i] * $b['duals'][$j];
  $new_duals[$pos] = $val;
  $j = $j + 1;
};
  $i = $i + 1;
};
  $k = 0;
  while ($k < _len($a['duals'])) {
  $val = $new_duals[$k] + $a['duals'][$k] * $b['real'];
  $new_duals[$k] = $val;
  $k = $k + 1;
};
  $l = 0;
  while ($l < _len($b['duals'])) {
  $val = $new_duals[$l] + $b['duals'][$l] * $a['real'];
  $new_duals[$l] = $val;
  $l = $l + 1;
};
  return ['real' => $a['real'] * $b['real'], 'duals' => $new_duals];
};
  function dual_mul_real($a, $b) {
  $ds = [];
  $i = 0;
  while ($i < _len($a['duals'])) {
  $ds = _append($ds, $a['duals'][$i] * $b);
  $i = $i + 1;
};
  return ['real' => $a['real'] * $b, 'duals' => $ds];
};
  function dual_pow($x, $n) {
  if ($n < 0) {
  $panic('power must be a positive integer');
}
  if ($n == 0) {
  return ['real' => 1.0, 'duals' => []];
}
  $res = $x;
  $i = 1;
  while ($i < $n) {
  $res = dual_mul($res, $x);
  $i = $i + 1;
};
  return $res;
};
  function factorial($n) {
  $res = 1.0;
  $i = 2;
  while ($i <= $n) {
  $res = $res * (floatval($i));
  $i = $i + 1;
};
  return $res;
};
  function differentiate($func, $position, $order) {
  $d = make_dual($position, 1);
  $result = $func($d);
  if ($order == 0) {
  return $result['real'];
}
  return $result['duals'][$order - 1] * factorial($order);
};
  function test_differentiate() {
  $f1 = null;
$f1 = function($x) use (&$f1) {
  return dual_pow($x, 2);
};
  if (differentiate($f1, 2.0, 2) != 2.0) {
  $panic('f1 failed');
}
  $f2 = null;
$f2 = function($x) use (&$f2, $f1) {
  return dual_mul(dual_pow($x, 2), dual_pow($x, 4));
};
  if (differentiate($f2, 9.0, 2) != 196830.0) {
  $panic('f2 failed');
}
  $f3 = null;
$f3 = function($y) use (&$f3, $f1, $f2) {
  return dual_mul_real(dual_pow(dual_add_real($y, 3.0), 6), 0.5);
};
  if (differentiate($f3, 3.5, 4) != 7605.0) {
  $panic('f3 failed');
}
  $f4 = null;
$f4 = function($y) use (&$f4, $f1, $f2, $f3) {
  return dual_pow($y, 2);
};
  if (differentiate($f4, 4.0, 3) != 0.0) {
  $panic('f4 failed');
}
};
  function main() {
  test_differentiate();
  $f = null;
$f = function($y) use (&$f) {
  return dual_mul(dual_pow($y, 2), dual_pow($y, 4));
};
  $res = differentiate($f, 9.0, 2);
  echo rtrim(json_encode($res, 1344)), PHP_EOL;
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
