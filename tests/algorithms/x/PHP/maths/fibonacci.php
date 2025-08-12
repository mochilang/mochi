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
  function mochi_sqrt($x) {
  global $fib_cache_global, $fib_memo_cache;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = _iadd($i, 1);
};
  return $guess;
};
  function powf($x, $n) {
  global $fib_cache_global, $fib_memo_cache;
  $res = 1.0;
  $i = 0;
  while ($i < $n) {
  $res = $res * $x;
  $i = _iadd($i, 1);
};
  return $res;
};
  function roundf($x) {
  global $fib_cache_global, $fib_memo_cache;
  if ($x >= 0.0) {
  return intval(($x + 0.5));
}
  return intval(($x - 0.5));
};
  function fib_iterative($n) {
  global $fib_cache_global, $fib_memo_cache;
  if ($n < 0) {
  _panic('n is negative');
}
  if ($n == 0) {
  return [0];
}
  $fib = [0, 1];
  $i = 2;
  while ($i <= $n) {
  $fib = _append($fib, _iadd($fib[_isub($i, 1)], $fib[_isub($i, 2)]));
  $i = _iadd($i, 1);
};
  return $fib;
};
  function fib_recursive_term($i) {
  global $fib_cache_global, $fib_memo_cache;
  if ($i < 0) {
  _panic('n is negative');
}
  if ($i < 2) {
  return $i;
}
  return _iadd(fib_recursive_term(_isub($i, 1)), fib_recursive_term(_isub($i, 2)));
};
  function fib_recursive($n) {
  global $fib_cache_global, $fib_memo_cache;
  if ($n < 0) {
  _panic('n is negative');
}
  $res = [];
  $i = 0;
  while ($i <= $n) {
  $res = _append($res, fib_recursive_term($i));
  $i = _iadd($i, 1);
};
  return $res;
};
  $fib_cache_global = [];
  function fib_recursive_cached_term($i) {
  global $fib_cache_global, $fib_memo_cache;
  if ($i < 0) {
  _panic('n is negative');
}
  if ($i < 2) {
  return $i;
}
  if (array_key_exists($i, $fib_cache_global)) {
  return $fib_cache_global[$i];
}
  $val = _iadd(fib_recursive_cached_term(_isub($i, 1)), fib_recursive_cached_term(_isub($i, 2)));
  $fib_cache_global[$i] = $val;
  return $val;
};
  function fib_recursive_cached($n) {
  global $fib_cache_global, $fib_memo_cache;
  if ($n < 0) {
  _panic('n is negative');
}
  $res = [];
  $j = 0;
  while ($j <= $n) {
  $res = _append($res, fib_recursive_cached_term($j));
  $j = _iadd($j, 1);
};
  return $res;
};
  $fib_memo_cache = [0 => 0, 1 => 1, 2 => 1];
  function fib_memoization_term($num) {
  global $fib_cache_global, $fib_memo_cache;
  if (array_key_exists($num, $fib_memo_cache)) {
  return $fib_memo_cache[$num];
}
  $value = _iadd(fib_memoization_term(_isub($num, 1)), fib_memoization_term(_isub($num, 2)));
  $fib_memo_cache[$num] = $value;
  return $value;
};
  function fib_memoization($n) {
  global $fib_cache_global, $fib_memo_cache;
  if ($n < 0) {
  _panic('n is negative');
}
  $out = [];
  $i = 0;
  while ($i <= $n) {
  $out = _append($out, fib_memoization_term($i));
  $i = _iadd($i, 1);
};
  return $out;
};
  function fib_binet($n) {
  global $fib_cache_global, $fib_memo_cache;
  if ($n < 0) {
  _panic('n is negative');
}
  if ($n >= 1475) {
  _panic('n is too large');
}
  $sqrt5 = mochi_sqrt(5.0);
  $phi = (1.0 + $sqrt5) / 2.0;
  $res = [];
  $i = 0;
  while ($i <= $n) {
  $val = roundf(powf($phi, $i) / $sqrt5);
  $res = _append($res, $val);
  $i = _iadd($i, 1);
};
  return $res;
};
  function matrix_mul($a, $b) {
  global $fib_cache_global, $fib_memo_cache;
  $a00 = _iadd(_imul($a[0][0], $b[0][0]), _imul($a[0][1], $b[1][0]));
  $a01 = _iadd(_imul($a[0][0], $b[0][1]), _imul($a[0][1], $b[1][1]));
  $a10 = _iadd(_imul($a[1][0], $b[0][0]), _imul($a[1][1], $b[1][0]));
  $a11 = _iadd(_imul($a[1][0], $b[0][1]), _imul($a[1][1], $b[1][1]));
  return [[$a00, $a01], [$a10, $a11]];
};
  function matrix_pow($m, $power) {
  global $fib_cache_global, $fib_memo_cache;
  if ($power < 0) {
  _panic('power is negative');
}
  $result = [[1, 0], [0, 1]];
  $base = $m;
  $p = $power;
  while ($p > 0) {
  if (_imod($p, 2) == 1) {
  $result = matrix_mul($result, $base);
}
  $base = matrix_mul($base, $base);
  $p = intval((_intdiv($p, 2)));
};
  return $result;
};
  function fib_matrix($n) {
  global $fib_cache_global, $fib_memo_cache;
  if ($n < 0) {
  _panic('n is negative');
}
  if ($n == 0) {
  return 0;
}
  $m = [[1, 1], [1, 0]];
  $res = matrix_pow($m, _isub($n, 1));
  return $res[0][0];
};
  function run_tests() {
  global $fib_cache_global, $fib_memo_cache;
  $expected = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55];
  $it = fib_iterative(10);
  $rec = fib_recursive(10);
  $cache = fib_recursive_cached(10);
  $memo = fib_memoization(10);
  $bin = fib_binet(10);
  $m = fib_matrix(10);
  if ($it != $expected) {
  _panic('iterative failed');
}
  if ($rec != $expected) {
  _panic('recursive failed');
}
  if ($cache != $expected) {
  _panic('cached failed');
}
  if ($memo != $expected) {
  _panic('memoization failed');
}
  if ($bin != $expected) {
  _panic('binet failed');
}
  if ($m != 55) {
  _panic('matrix failed');
}
  return $m;
};
  echo rtrim(_str(run_tests())), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
