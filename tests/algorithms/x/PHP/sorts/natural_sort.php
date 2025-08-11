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
  $DIGITS = '0123456789';
  $LOWER = 'abcdefghijklmnopqrstuvwxyz';
  $UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  function index_of($s, $ch) {
  global $DIGITS, $LOWER, $UPPER, $example1, $example2;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
};
  function is_digit($ch) {
  global $DIGITS, $LOWER, $UPPER, $example1, $example2;
  return index_of($DIGITS, $ch) >= 0;
};
  function to_lower($ch) {
  global $DIGITS, $LOWER, $UPPER, $example1, $example2;
  $idx = index_of($UPPER, $ch);
  if ($idx >= 0) {
  return substr($LOWER, $idx, _iadd($idx, 1) - $idx);
}
  return $ch;
};
  function pad_left($s, $width) {
  global $DIGITS, $LOWER, $UPPER, $example1, $example2;
  $res = $s;
  while (strlen($res) < $width) {
  $res = '0' . $res;
};
  return $res;
};
  function alphanum_key($s) {
  global $DIGITS, $LOWER, $UPPER, $example1, $example2;
  $key = [];
  $i = 0;
  while ($i < strlen($s)) {
  if (is_digit(substr($s, $i, $i + 1 - $i))) {
  $num = '';
  while ($i < strlen($s) && is_digit(substr($s, $i, $i + 1 - $i))) {
  $num = $num . substr($s, $i, $i + 1 - $i);
  $i = _iadd($i, 1);
};
  $len_str = pad_left(_str(strlen($num)), 3);
  $key = _append($key, '#' . $len_str . $num);
} else {
  $seg = '';
  while ($i < strlen($s)) {
  if (is_digit(substr($s, $i, $i + 1 - $i))) {
  break;
}
  $seg = $seg . to_lower(substr($s, $i, $i + 1 - $i));
  $i = _iadd($i, 1);
};
  $key = _append($key, $seg);
}
};
  return $key;
};
  function compare_keys($a, $b) {
  global $DIGITS, $LOWER, $UPPER, $example1, $example2;
  $i = 0;
  while ($i < count($a) && $i < count($b)) {
  if ($a[$i] < $b[$i]) {
  return -1;
}
  if ($a[$i] > $b[$i]) {
  return 1;
}
  $i = _iadd($i, 1);
};
  if (count($a) < count($b)) {
  return -1;
}
  if (count($a) > count($b)) {
  return 1;
}
  return 0;
};
  function natural_sort($arr) {
  global $DIGITS, $LOWER, $UPPER, $example1, $example2;
  $res = [];
  $keys = [];
  $k = 0;
  while ($k < count($arr)) {
  $res = _append($res, $arr[$k]);
  $keys = _append($keys, alphanum_key($arr[$k]));
  $k = _iadd($k, 1);
};
  $i = 1;
  while ($i < count($res)) {
  $current = $res[$i];
  $current_key = $keys[$i];
  $j = _isub($i, 1);
  while ($j >= 0 && compare_keys($keys[$j], $current_key) > 0) {
  $res[_iadd($j, 1)] = $res[$j];
  $keys[_iadd($j, 1)] = $keys[$j];
  $j = _isub($j, 1);
};
  $res[_iadd($j, 1)] = $current;
  $keys[_iadd($j, 1)] = $current_key;
  $i = _iadd($i, 1);
};
  return $res;
};
  $example1 = ['2 ft 7 in', '1 ft 5 in', '10 ft 2 in', '2 ft 11 in', '7 ft 6 in'];
  echo rtrim(_str(natural_sort($example1))), PHP_EOL;
  $example2 = ['Elm11', 'Elm12', 'Elm2', 'elm0', 'elm1', 'elm10', 'elm13', 'elm9'];
  echo rtrim(_str(natural_sort($example2))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
