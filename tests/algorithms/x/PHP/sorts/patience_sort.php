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
  function bisect_left($stacks, $value) {
  $low = 0;
  $high = count($stacks);
  while ($low < $high) {
  $mid = _intdiv((_iadd($low, $high)), 2);
  $stack = $stacks[$mid];
  $top_idx = _isub(count($stack), 1);
  $top = $stack[$top_idx];
  if ($top < $value) {
  $low = _iadd($mid, 1);
} else {
  $high = $mid;
}
};
  return $low;
};
  function reverse_list($src) {
  $res = [];
  $i = _isub(count($src), 1);
  while ($i >= 0) {
  $res = _append($res, $src[$i]);
  $i = _isub($i, 1);
};
  return $res;
};
  function patience_sort($collection) {
  $stacks = [];
  $i = 0;
  while ($i < count($collection)) {
  $element = $collection[$i];
  $idx = bisect_left($stacks, $element);
  if ($idx != count($stacks)) {
  $stack = $stacks[$idx];
  $stacks[$idx] = _append($stack, $element);
} else {
  $new_stack = [$element];
  $stacks = _append($stacks, $new_stack);
}
  $i = _iadd($i, 1);
};
  $i = 0;
  while ($i < count($stacks)) {
  $stacks[$i] = reverse_list($stacks[$i]);
  $i = _iadd($i, 1);
};
  $indices = [];
  $i = 0;
  while ($i < count($stacks)) {
  $indices = _append($indices, 0);
  $i = _iadd($i, 1);
};
  $total = 0;
  $i = 0;
  while ($i < count($stacks)) {
  $total = _iadd($total, count($stacks[$i]));
  $i = _iadd($i, 1);
};
  $result = [];
  $count = 0;
  while ($count < $total) {
  $min_val = 0;
  $min_stack = -1;
  $j = 0;
  while ($j < count($stacks)) {
  $idx = $indices[$j];
  if ($idx < count($stacks[$j])) {
  $val = $stacks[$j][$idx];
  if ($min_stack < 0) {
  $min_val = $val;
  $min_stack = $j;
} else {
  if ($val < $min_val) {
  $min_val = $val;
  $min_stack = $j;
};
};
}
  $j = _iadd($j, 1);
};
  $result = _append($result, $min_val);
  $indices[$min_stack] = _iadd($indices[$min_stack], 1);
  $count = _iadd($count, 1);
};
  $i = 0;
  while ($i < count($result)) {
  $collection[$i] = $result[$i];
  $i = _iadd($i, 1);
};
  return $collection;
};
  echo rtrim(_str(patience_sort([1, 9, 5, 21, 17, 6]))), PHP_EOL;
  echo rtrim(_str(patience_sort([]))), PHP_EOL;
  echo rtrim(_str(patience_sort([-3, -17, -48]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
