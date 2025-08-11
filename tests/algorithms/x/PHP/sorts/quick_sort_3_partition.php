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
  function quick_sort_3partition($arr, $left, $right) {
  global $array1, $array2, $array3, $nums1, $nums2, $nums3;
  if ($right <= $left) {
  return $arr;
}
  $a = $left;
  $i = $left;
  $b = $right;
  $pivot = $arr[$left];
  while ($i <= $b) {
  if ($arr[$i] < $pivot) {
  $temp = $arr[$a];
  $arr[$a] = $arr[$i];
  $arr[$i] = $temp;
  $a = _iadd($a, 1);
  $i = _iadd($i, 1);
} else {
  if ($arr[$i] > $pivot) {
  $temp = $arr[$b];
  $arr[$b] = $arr[$i];
  $arr[$i] = $temp;
  $b = _isub($b, 1);
} else {
  $i = _iadd($i, 1);
};
}
};
  $arr = quick_sort_3partition($arr, $left, _isub($a, 1));
  $arr = quick_sort_3partition($arr, _iadd($b, 1), $right);
  return $arr;
};
  function quick_sort_lomuto_partition($arr, $left, $right) {
  global $array1, $array2, $array3, $nums1, $nums2, $nums3;
  if ($left < $right) {
  $pivot_index = lomuto_partition($arr, $left, $right);
  $arr = quick_sort_lomuto_partition($arr, $left, _isub($pivot_index, 1));
  $arr = quick_sort_lomuto_partition($arr, _iadd($pivot_index, 1), $right);
}
  return $arr;
};
  function lomuto_partition(&$arr, $left, $right) {
  global $array1, $array2, $array3, $nums1, $nums2, $nums3;
  $pivot = $arr[$right];
  $store_index = $left;
  $i = $left;
  while ($i < $right) {
  if ($arr[$i] < $pivot) {
  $temp = $arr[$store_index];
  $arr[$store_index] = $arr[$i];
  $arr[$i] = $temp;
  $store_index = _iadd($store_index, 1);
}
  $i = _iadd($i, 1);
};
  $temp = $arr[$right];
  $arr[$right] = $arr[$store_index];
  $arr[$store_index] = $temp;
  return $store_index;
};
  function three_way_radix_quicksort($arr) {
  global $array1, $array2, $array3, $nums1, $nums2, $nums3;
  if (count($arr) <= 1) {
  return $arr;
}
  $pivot = $arr[0];
  $less = [];
  $equal = [];
  $greater = [];
  $i = 0;
  while ($i < count($arr)) {
  $val = $arr[$i];
  if ($val < $pivot) {
  $less = _append($less, $val);
} else {
  if ($val > $pivot) {
  $greater = _append($greater, $val);
} else {
  $equal = _append($equal, $val);
};
}
  $i = _iadd($i, 1);
};
  $sorted_less = three_way_radix_quicksort($less);
  $sorted_greater = three_way_radix_quicksort($greater);
  $result = array_merge($sorted_less, $equal);
  $result = array_merge($result, $sorted_greater);
  return $result;
};
  $array1 = [5, -1, -1, 5, 5, 24, 0];
  $array1 = quick_sort_3partition($array1, 0, _isub(count($array1), 1));
  echo rtrim(_str($array1)), PHP_EOL;
  $array2 = [9, 0, 2, 6];
  $array2 = quick_sort_3partition($array2, 0, _isub(count($array2), 1));
  echo rtrim(_str($array2)), PHP_EOL;
  $array3 = [];
  $array3 = quick_sort_3partition($array3, 0, _isub(count($array3), 1));
  echo rtrim(_str($array3)), PHP_EOL;
  $nums1 = [0, 5, 3, 1, 2];
  $nums1 = quick_sort_lomuto_partition($nums1, 0, _isub(count($nums1), 1));
  echo rtrim(_str($nums1)), PHP_EOL;
  $nums2 = [];
  $nums2 = quick_sort_lomuto_partition($nums2, 0, _isub(count($nums2), 1));
  echo rtrim(_str($nums2)), PHP_EOL;
  $nums3 = [-2, 5, 0, -4];
  $nums3 = quick_sort_lomuto_partition($nums3, 0, _isub(count($nums3), 1));
  echo rtrim(_str($nums3)), PHP_EOL;
  echo rtrim(_str(three_way_radix_quicksort([]))), PHP_EOL;
  echo rtrim(_str(three_way_radix_quicksort([1]))), PHP_EOL;
  echo rtrim(_str(three_way_radix_quicksort([-5, -2, 1, -2, 0, 1]))), PHP_EOL;
  echo rtrim(_str(three_way_radix_quicksort([1, 2, 5, 1, 2, 0, 0, 5, 2, -1]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
