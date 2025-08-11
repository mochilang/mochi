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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
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
  function search($target, $arr) {
  $left = 0;
  $right = _isub(count($arr), 1);
  while ($left <= $right) {
  $middle = _intdiv((_iadd($left, $right)), 2);
  if ($arr[$middle] == $target) {
  return true;
}
  if ($arr[$middle] < $target) {
  $left = _iadd($middle, 1);
} else {
  $right = _isub($middle, 1);
}
};
  return false;
};
  function sort_int($xs) {
  $arr = $xs;
  $i = 0;
  while ($i < count($arr)) {
  $j = _iadd($i, 1);
  while ($j < count($arr)) {
  if ($arr[$j] < $arr[$i]) {
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
}
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  return $arr;
};
  function permutations_of_number($n) {
  $s = _str($n);
  $d = [];
  $i = 0;
  while ($i < strlen($s)) {
  $d = _append($d, intval($s[$i]));
  $i = _iadd($i, 1);
};
  $res = [];
  $a = 0;
  while ($a < count($d)) {
  $b = 0;
  while ($b < count($d)) {
  if ($b != $a) {
  $c = 0;
  while ($c < count($d)) {
  if ($c != $a && $c != $b) {
  $e = 0;
  while ($e < count($d)) {
  if ($e != $a && $e != $b && $e != $c) {
  $val = _iadd(_iadd(_iadd(_imul($d[$a], 1000), _imul($d[$b], 100)), _imul($d[$c], 10)), $d[$e]);
  $res = _append($res, $val);
}
  $e = _iadd($e, 1);
};
}
  $c = _iadd($c, 1);
};
}
  $b = _iadd($b, 1);
};
  $a = _iadd($a, 1);
};
  return $res;
};
  function abs_int($x) {
  if ($x < 0) {
  return -$x;
}
  return $x;
};
  function contains_int($xs, $v) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $v) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
};
  function solution() {
  $prime_list = [];
  $n = 1001;
  while ($n < 10000) {
  if (is_prime($n)) {
  $prime_list = _append($prime_list, $n);
}
  $n = _iadd($n, 2);
};
  $candidates = [];
  $i = 0;
  while ($i < count($prime_list)) {
  $number = $prime_list[$i];
  $tmp = [];
  $perms = permutations_of_number($number);
  $j = 0;
  while ($j < count($perms)) {
  $prime = $perms[$j];
  if (_imod($prime, 2) != 0 && search($prime, $prime_list)) {
  $tmp = _append($tmp, $prime);
}
  $j = _iadd($j, 1);
};
  $tmp = sort_int($tmp);
  if (count($tmp) >= 3) {
  $candidates = _append($candidates, $tmp);
}
  $i = _iadd($i, 1);
};
  $passed = [];
  $i = 0;
  while ($i < count($candidates)) {
  $candidate = $candidates[$i];
  $found = false;
  $a = 0;
  while ($a < count($candidate)) {
  $b = _iadd($a, 1);
  while ($b < count($candidate)) {
  $c = _iadd($b, 1);
  while ($c < count($candidate)) {
  $x = $candidate[$a];
  $y = $candidate[$b];
  $z = $candidate[$c];
  if (abs_int(_isub($x, $y)) == abs_int(_isub($y, $z)) && $x != $y && $x != $z && $y != $z) {
  $triple = sort_int([$x, $y, $z]);
  $passed = _append($passed, $triple);
  $found = true;
  break;
}
  $c = _iadd($c, 1);
};
  if ($found) {
  break;
}
  $b = _iadd($b, 1);
};
  if ($found) {
  break;
}
  $a = _iadd($a, 1);
};
  $i = _iadd($i, 1);
};
  $answer_nums = [];
  $i = 0;
  while ($i < count($passed)) {
  $seq = $passed[$i];
  $val = intval(_str($seq[0]) . _str($seq[1]) . _str($seq[2]));
  if (!contains_int($answer_nums, $val)) {
  $answer_nums = _append($answer_nums, $val);
}
  $i = _iadd($i, 1);
};
  $max_val = $answer_nums[0];
  $i = 1;
  while ($i < count($answer_nums)) {
  if ($answer_nums[$i] > $max_val) {
  $max_val = $answer_nums[$i];
}
  $i = _iadd($i, 1);
};
  return $max_val;
};
  echo rtrim(json_encode(solution(), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
