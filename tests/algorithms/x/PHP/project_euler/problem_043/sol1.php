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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function is_substring_divisible($num) {
  if (_imod($num[3], 2) != 0) {
  return false;
}
  if (_imod((_iadd(_iadd($num[2], $num[3]), $num[4])), 3) != 0) {
  return false;
}
  if (_imod($num[5], 5) != 0) {
  return false;
}
  $primes = [7, 11, 13, 17];
  $i = 0;
  while ($i < count($primes)) {
  $p = $primes[$i];
  $idx = _iadd($i, 4);
  $val = _iadd(_iadd(_imul($num[$idx], 100), _imul($num[_iadd($idx, 1)], 10)), $num[_iadd($idx, 2)]);
  if (_imod($val, $p) != 0) {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
};
  function remove_at($xs, $idx) {
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i != $idx) {
  $res = _append($res, $xs[$i]);
}
  $i = _iadd($i, 1);
};
  return $res;
};
  function digits_to_number($xs) {
  $value = 0;
  $i = 0;
  while ($i < count($xs)) {
  $value = _iadd(_imul($value, 10), $xs[$i]);
  $i = _iadd($i, 1);
};
  return $value;
};
  function search($prefix, $remaining) {
  if (count($remaining) == 0) {
  if (is_substring_divisible($prefix)) {
  return digits_to_number($prefix);
};
  return 0;
}
  $total = 0;
  $i = 0;
  while ($i < count($remaining)) {
  $d = $remaining[$i];
  $next_prefix = _append($prefix, $d);
  $next_remaining = remove_at($remaining, $i);
  $total = _iadd($total, search($next_prefix, $next_remaining));
  $i = _iadd($i, 1);
};
  return $total;
};
  function solution($n) {
  $digits = [];
  $i = 0;
  while ($i < $n) {
  $digits = _append($digits, $i);
  $i = _iadd($i, 1);
};
  return search([], $digits);
};
  echo rtrim('solution() =') . " " . rtrim(json_encode(solution(10), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
