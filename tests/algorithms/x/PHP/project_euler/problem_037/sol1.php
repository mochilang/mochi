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
  function is_prime($number) {
  if (1 < $number && $number < 4) {
  return true;
}
  if ($number < 2 || _imod($number, 2) == 0 || _imod($number, 3) == 0) {
  return false;
}
  $i = 5;
  while (_imul($i, $i) <= $number) {
  if (_imod($number, $i) == 0 || _imod($number, (_iadd($i, 2))) == 0) {
  return false;
}
  $i = _iadd($i, 6);
};
  return true;
};
  function list_truncated_nums($n) {
  $str_num = _str($n);
  $list_nums = [$n];
  $i = 1;
  $length = strlen($str_num);
  while ($i < $length) {
  $right = intval(substr($str_num, $i, $length - $i));
  $left = intval(substr($str_num, 0, _isub($length, $i)));
  $list_nums = _append($list_nums, $right);
  $list_nums = _append($list_nums, $left);
  $i = _iadd($i, 1);
};
  return $list_nums;
};
  function validate($n) {
  $s = _str($n);
  $length = strlen($s);
  if ($length > 3) {
  $last3 = intval(substr($s, _isub($length, 3), $length - _isub($length, 3)));
  $first3 = intval(substr($s, 0, 3));
  if (!(is_prime($last3) && is_prime($first3))) {
  return false;
};
}
  return true;
};
  function compute_truncated_primes($count) {
  $list_truncated_primes = [];
  $num = 13;
  while (count($list_truncated_primes) != $count) {
  if (validate($num)) {
  $list_nums = list_truncated_nums($num);
  $all_prime = true;
  $j = 0;
  while ($j < count($list_nums)) {
  if (!is_prime($list_nums[$j])) {
  $all_prime = false;
  break;
}
  $j = _iadd($j, 1);
};
  if ($all_prime) {
  $list_truncated_primes = _append($list_truncated_primes, $num);
};
}
  $num = _iadd($num, 2);
};
  return $list_truncated_primes;
};
  function solution() {
  $primes = compute_truncated_primes(11);
  $total = 0;
  $i = 0;
  while ($i < count($primes)) {
  $total = _iadd($total, $primes[$i]);
  $i = _iadd($i, 1);
};
  return $total;
};
  echo rtrim('sum(compute_truncated_primes(11)) = ' . _str(solution())), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
