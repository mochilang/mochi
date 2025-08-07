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
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function identity($n) {
  $i = 0;
  $mat = [];
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  if ($i == $j) {
  $row = _append($row, 1);
} else {
  $row = _append($row, 0);
}
  $j = $j + 1;
};
  $mat = _append($mat, $row);
  $i = $i + 1;
};
  return $mat;
};
  function matrix_mul($a, $b) {
  $n = count($a);
  $result = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $cell = 0;
  $k = 0;
  while ($k < $n) {
  $cell = $cell + $a[$i][$k] * $b[$k][$j];
  $k = $k + 1;
};
  $row = _append($row, $cell);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
};
  function matrix_pow($base, $exp) {
  $result = identity(count($base));
  $b = $base;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = matrix_mul($result, $b);
}
  $b = matrix_mul($b, $b);
  $e = _intdiv($e, 2);
};
  return $result;
};
  function fibonacci_with_matrix_exponentiation($n, $f1, $f2) {
  if ($n == 1) {
  return $f1;
}
  if ($n == 2) {
  return $f2;
}
  $base = [[1, 1], [1, 0]];
  $m = matrix_pow($base, $n - 2);
  return $f2 * $m[0][0] + $f1 * $m[0][1];
};
  function simple_fibonacci($n, $f1, $f2) {
  if ($n == 1) {
  return $f1;
}
  if ($n == 2) {
  return $f2;
}
  $a = $f1;
  $b = $f2;
  $count = $n - 2;
  while ($count > 0) {
  $tmp = $a + $b;
  $a = $b;
  $b = $tmp;
  $count = $count - 1;
};
  return $b;
};
  echo rtrim(_str(fibonacci_with_matrix_exponentiation(1, 5, 6))), PHP_EOL;
  echo rtrim(_str(fibonacci_with_matrix_exponentiation(2, 10, 11))), PHP_EOL;
  echo rtrim(_str(fibonacci_with_matrix_exponentiation(13, 0, 1))), PHP_EOL;
  echo rtrim(_str(fibonacci_with_matrix_exponentiation(10, 5, 9))), PHP_EOL;
  echo rtrim(_str(fibonacci_with_matrix_exponentiation(9, 2, 3))), PHP_EOL;
  echo rtrim(_str(simple_fibonacci(1, 5, 6))), PHP_EOL;
  echo rtrim(_str(simple_fibonacci(2, 10, 11))), PHP_EOL;
  echo rtrim(_str(simple_fibonacci(13, 0, 1))), PHP_EOL;
  echo rtrim(_str(simple_fibonacci(10, 5, 9))), PHP_EOL;
  echo rtrim(_str(simple_fibonacci(9, 2, 3))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
