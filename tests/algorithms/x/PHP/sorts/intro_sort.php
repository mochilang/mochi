<?php
ini_set('memory_limit', '-1');
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
function insertion_sort($a, $start, $end_) {
  global $example1, $example2;
  $arr = $a;
  $i = $start;
  while ($i < $end_) {
  $key = $arr[$i];
  $j = $i;
  while ($j > $start && $arr[_isub($j, 1)] > $key) {
  $arr[$j] = $arr[_isub($j, 1)];
  $j = _isub($j, 1);
};
  $arr[$j] = $key;
  $i = _iadd($i, 1);
};
  return $arr;
}
function heapify($a, $index, $heap_size) {
  global $example1, $example2;
  $arr = $a;
  $largest = $index;
  $left = _iadd(_imul(2, $index), 1);
  $right = _iadd(_imul(2, $index), 2);
  if ($left < $heap_size && $arr[$left] > $arr[$largest]) {
  $largest = $left;
}
  if ($right < $heap_size && $arr[$right] > $arr[$largest]) {
  $largest = $right;
}
  if ($largest != $index) {
  $temp = $arr[$index];
  $arr[$index] = $arr[$largest];
  $arr[$largest] = $temp;
  $arr = heapify($arr, $largest, $heap_size);
}
  return $arr;
}
function heap_sort($a) {
  global $example1, $example2;
  $arr = $a;
  $n = count($arr);
  if ($n <= 1) {
  return $arr;
}
  $i = _intdiv($n, 2);
  while (true) {
  $arr = heapify($arr, $i, $n);
  if ($i == 0) {
  break;
}
  $i = _isub($i, 1);
};
  $i = _isub($n, 1);
  while ($i > 0) {
  $temp = $arr[0];
  $arr[0] = $arr[$i];
  $arr[$i] = $temp;
  $arr = heapify($arr, 0, $i);
  $i = _isub($i, 1);
};
  return $arr;
}
function median_of_3($arr, $first, $middle, $last) {
  global $example1, $example2;
  $a = $arr[$first];
  $b = $arr[$middle];
  $c = $arr[$last];
  if (($a > $b && $a < $c) || ($a < $b && $a > $c)) {
  return $a;
} else {
  if (($b > $a && $b < $c) || ($b < $a && $b > $c)) {
  return $b;
} else {
  return $c;
};
}
}
function partition(&$arr, $low, $high, $pivot) {
  global $example1, $example2;
  $i = $low;
  $j = $high;
  while (true) {
  while ($arr[$i] < $pivot) {
  $i = _iadd($i, 1);
};
  $j = _isub($j, 1);
  while ($pivot < $arr[$j]) {
  $j = _isub($j, 1);
};
  if ($i >= $j) {
  return $i;
}
  $temp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $temp;
  $i = _iadd($i, 1);
};
}
function int_log2($n) {
  global $example1, $example2;
  $v = $n;
  $r = 0;
  while ($v > 1) {
  $v = _intdiv($v, 2);
  $r = _iadd($r, 1);
};
  return $r;
}
function intro_sort($arr, $start, $end_, $size_threshold, $max_depth) {
  global $example1, $example2;
  $array = $arr;
  $s = $start;
  $e = $end_;
  $depth = $max_depth;
  while (_isub($e, $s) > $size_threshold) {
  if ($depth == 0) {
  return heap_sort($array);
}
  $depth = _isub($depth, 1);
  $pivot = median_of_3($array, $s, _iadd(_iadd($s, (_intdiv((_isub($e, $s)), 2))), 1), _isub($e, 1));
  $p = partition($array, $s, $e, $pivot);
  $array = intro_sort($array, $p, $e, $size_threshold, $depth);
  $e = $p;
};
  $res = insertion_sort($array, $s, $e);
  $_ = count($res);
  return $res;
}
function intro_sort_main($arr) {
  global $example1, $example2;
  if (count($arr) == 0) {
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($arr, 1344)))))), PHP_EOL;
  return;
}
  $max_depth = _imul(2, int_log2(count($arr)));
  $sorted = intro_sort($arr, 0, count($arr), 16, $max_depth);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($sorted, 1344)))))), PHP_EOL;
}
$example1 = [4, 2, 6, 8, 1, 7, 8, 22, 14, 56, 27, 79, 23, 45, 14, 12];
intro_sort_main($example1);
$example2 = [21, 15, 11, 45, -2, -11, 46];
intro_sort_main($example2);
