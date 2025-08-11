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
  function swap(&$seq, $i, $j) {
  global $seq1, $seq2, $seq3, $seq4, $seq5, $seq6, $seq7, $seq8;
  $temp = $seq[$i];
  $seq[$i] = $seq[$j];
  $seq[$j] = $temp;
};
  function slowsort_recursive(&$seq, $start, $end_index) {
  global $seq1, $seq2, $seq3, $seq4, $seq5, $seq6, $seq7, $seq8;
  if ($start >= $end_index) {
  return;
}
  $mid = _intdiv((_iadd($start, $end_index)), 2);
  slowsort_recursive($seq, $start, $mid);
  slowsort_recursive($seq, _iadd($mid, 1), $end_index);
  if ($seq[$end_index] < $seq[$mid]) {
  swap($seq, $end_index, $mid);
}
  slowsort_recursive($seq, $start, _isub($end_index, 1));
};
  function slow_sort(&$seq) {
  global $seq1, $seq2, $seq3, $seq4, $seq5, $seq6, $seq7, $seq8;
  if (count($seq) > 0) {
  slowsort_recursive($seq, 0, _isub(count($seq), 1));
}
  return $seq;
};
  $seq1 = [1, 6, 2, 5, 3, 4, 4, 5];
  echo rtrim(_str(slow_sort($seq1))), PHP_EOL;
  $seq2 = [];
  echo rtrim(_str(slow_sort($seq2))), PHP_EOL;
  $seq3 = [2];
  echo rtrim(_str(slow_sort($seq3))), PHP_EOL;
  $seq4 = [1, 2, 3, 4];
  echo rtrim(_str(slow_sort($seq4))), PHP_EOL;
  $seq5 = [4, 3, 2, 1];
  echo rtrim(_str(slow_sort($seq5))), PHP_EOL;
  $seq6 = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0];
  slowsort_recursive($seq6, 2, 7);
  echo rtrim(_str($seq6)), PHP_EOL;
  $seq7 = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0];
  slowsort_recursive($seq7, 0, 4);
  echo rtrim(_str($seq7)), PHP_EOL;
  $seq8 = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0];
  slowsort_recursive($seq8, 5, _isub(count($seq8), 1));
  echo rtrim(_str($seq8)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
