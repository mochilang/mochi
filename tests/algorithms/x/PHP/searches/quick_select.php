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
  function partition($data, $pivot) {
  $less = [];
  $equal = [];
  $greater = [];
  for ($i = 0; $i < count($data); $i++) {
  $v = $data[$i];
  if ($v < $pivot) {
  $less = _append($less, $v);
} else {
  if ($v > $pivot) {
  $greater = _append($greater, $v);
} else {
  $equal = _append($equal, $v);
};
}
};
  return [$less, $equal, $greater];
};
  function quick_select($items, $index) {
  if ($index < 0 || $index >= count($items)) {
  return -1;
}
  $pivot = $items[_idiv(count($items), 2)];
  $parts = partition($items, $pivot);
  $smaller = $parts[0];
  $equal = $parts[1];
  $larger = $parts[2];
  $count = count($equal);
  $m = count($smaller);
  if ($m <= $index && $index < _iadd($m, $count)) {
  return $pivot;
} else {
  if ($index < $m) {
  return quick_select($smaller, $index);
} else {
  return quick_select($larger, _isub($index, (_iadd($m, $count))));
};
}
};
  function median($items) {
  $n = count($items);
  $mid = _intdiv($n, 2);
  if (_imod($n, 2) != 0) {
  return _imul(1.0, quick_select($items, $mid));
} else {
  $low = quick_select($items, _isub($mid, 1));
  $high = quick_select($items, $mid);
  return _idiv((_imul(1.0, (_iadd($low, $high)))), 2.0);
}
};
  echo rtrim(_str(quick_select([2, 4, 5, 7, 899, 54, 32], 5))), PHP_EOL;
  echo rtrim(_str(quick_select([2, 4, 5, 7, 899, 54, 32], 1))), PHP_EOL;
  echo rtrim(_str(quick_select([5, 4, 3, 2], 2))), PHP_EOL;
  echo rtrim(_str(quick_select([3, 5, 7, 10, 2, 12], 3))), PHP_EOL;
  echo rtrim(_str(median([3, 2, 2, 9, 9]))), PHP_EOL;
  echo rtrim(_str(median([2, 2, 9, 9, 9, 3]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
