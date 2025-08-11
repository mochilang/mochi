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
  function fibonacci($k) {
  global $example1, $example2, $example3;
  if ($k < 0) {
  _panic('k must be >= 0');
}
  $a = 0;
  $b = 1;
  $i = 0;
  while ($i < $k) {
  $tmp = _iadd($a, $b);
  $a = $b;
  $b = $tmp;
  $i = _iadd($i, 1);
};
  return $a;
};
  function min_int($a, $b) {
  global $example1, $example2, $example3;
  if ($a < $b) {
  return $a;
} else {
  return $b;
}
};
  function fibonacci_search($arr, $val) {
  global $example1, $example2, $example3;
  $n = count($arr);
  $m = 0;
  while (fibonacci($m) < $n) {
  $m = _iadd($m, 1);
};
  $offset = 0;
  while ($m > 0) {
  $i = min_int(_iadd($offset, fibonacci(_isub($m, 1))), _isub($n, 1));
  $item = $arr[$i];
  if ($item == $val) {
  return $i;
} else {
  if ($val < $item) {
  $m = _isub($m, 1);
} else {
  $offset = _iadd($offset, fibonacci(_isub($m, 1)));
  $m = _isub($m, 2);
};
}
};
  return -1;
};
  $example1 = [4, 5, 6, 7];
  $example2 = [-18, 2];
  $example3 = [0, 5, 10, 15, 20, 25, 30];
  echo rtrim(_str(fibonacci_search($example1, 4))), PHP_EOL;
  echo rtrim(_str(fibonacci_search($example1, -10))), PHP_EOL;
  echo rtrim(_str(fibonacci_search($example2, -18))), PHP_EOL;
  echo rtrim(_str(fibonacci_search($example3, 15))), PHP_EOL;
  echo rtrim(_str(fibonacci_search($example3, 17))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
