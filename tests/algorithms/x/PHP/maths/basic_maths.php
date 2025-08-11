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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pow_int($base, $exp) {
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = _imul($result, $base);
  $i = _iadd($i, 1);
};
  return $result;
};
  function prime_factors($n) {
  if ($n <= 0) {
  _panic('Only positive integers have prime factors');
}
  $num = $n;
  $pf = [];
  while (_imod($num, 2) == 0) {
  $pf = _append($pf, 2);
  $num = _intdiv($num, 2);
};
  $i = 3;
  while (_imul($i, $i) <= $num) {
  while (_imod($num, $i) == 0) {
  $pf = _append($pf, $i);
  $num = _intdiv($num, $i);
};
  $i = _iadd($i, 2);
};
  if ($num > 2) {
  $pf = _append($pf, $num);
}
  return $pf;
};
  function number_of_divisors($n) {
  if ($n <= 0) {
  _panic('Only positive numbers are accepted');
}
  $num = $n;
  $div = 1;
  $temp = 1;
  while (_imod($num, 2) == 0) {
  $temp = _iadd($temp, 1);
  $num = _intdiv($num, 2);
};
  $div = _imul($div, $temp);
  $i = 3;
  while (_imul($i, $i) <= $num) {
  $temp = 1;
  while (_imod($num, $i) == 0) {
  $temp = _iadd($temp, 1);
  $num = _intdiv($num, $i);
};
  $div = _imul($div, $temp);
  $i = _iadd($i, 2);
};
  if ($num > 1) {
  $div = _imul($div, 2);
}
  return $div;
};
  function sum_of_divisors($n) {
  if ($n <= 0) {
  _panic('Only positive numbers are accepted');
}
  $num = $n;
  $s = 1;
  $temp = 1;
  while (_imod($num, 2) == 0) {
  $temp = _iadd($temp, 1);
  $num = _intdiv($num, 2);
};
  if ($temp > 1) {
  $s = _imul($s, (_intdiv((_isub(pow_int(2, $temp), 1)), (_isub(2, 1)))));
}
  $i = 3;
  while (_imul($i, $i) <= $num) {
  $temp = 1;
  while (_imod($num, $i) == 0) {
  $temp = _iadd($temp, 1);
  $num = _intdiv($num, $i);
};
  if ($temp > 1) {
  $s = _imul($s, (_intdiv((_isub(pow_int($i, $temp), 1)), (_isub($i, 1)))));
}
  $i = _iadd($i, 2);
};
  return $s;
};
  function contains($arr, $x) {
  $idx = 0;
  while ($idx < count($arr)) {
  if ($arr[$idx] == $x) {
  return true;
}
  $idx = _iadd($idx, 1);
};
  return false;
};
  function unique($arr) {
  $result = [];
  $idx = 0;
  while ($idx < count($arr)) {
  $v = $arr[$idx];
  if (!contains($result, $v)) {
  $result = _append($result, $v);
}
  $idx = _iadd($idx, 1);
};
  return $result;
};
  function euler_phi($n) {
  if ($n <= 0) {
  _panic('Only positive numbers are accepted');
}
  $s = $n;
  $factors = unique(prime_factors($n));
  $idx = 0;
  while ($idx < count($factors)) {
  $x = $factors[$idx];
  $s = _imul((_intdiv($s, $x)), (_isub($x, 1)));
  $idx = _iadd($idx, 1);
};
  return $s;
};
  echo rtrim(_str(prime_factors(100))), PHP_EOL;
  echo rtrim(_str(number_of_divisors(100))), PHP_EOL;
  echo rtrim(_str(sum_of_divisors(100))), PHP_EOL;
  echo rtrim(_str(euler_phi(100))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
