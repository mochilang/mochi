<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
    return intdiv(intval($a), intval($b));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_ord($ch) {
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $digits = '0123456789';
  $i = 0;
  while ($i < strlen($lower)) {
  if (substr($lower, $i, $i + 1 - $i) == $ch) {
  return 97 + $i;
}
  $i = $i + 1;
};
  $i = 0;
  while ($i < strlen($upper)) {
  if (substr($upper, $i, $i + 1 - $i) == $ch) {
  return 65 + $i;
}
  $i = $i + 1;
};
  $i = 0;
  while ($i < strlen($digits)) {
  if (substr($digits, $i, $i + 1 - $i) == $ch) {
  return 48 + $i;
}
  $i = $i + 1;
};
  if ($ch == ' ') {
  return 32;
}
  if ($ch == '_') {
  return 95;
}
  if ($ch == '.') {
  return 46;
}
  if ($ch == '\'') {
  return 39;
}
  return 0;
};
  function lshift($num, $k) {
  $result = $num;
  $i = 0;
  while ($i < $k) {
  $result = $result * 2;
  $i = $i + 1;
};
  return $result;
};
  function rshift($num, $k) {
  $result = $num;
  $i = 0;
  while ($i < $k) {
  $result = _intdiv(($result - ($result % 2)), 2);
  $i = $i + 1;
};
  return $result;
};
  function is_contains_unique_chars($input_str) {
  $bitmap = 0;
  $i = 0;
  while ($i < strlen($input_str)) {
  $code = mochi_ord(substr($input_str, $i, $i + 1 - $i));
  if (fmod(rshift($bitmap, $code), 2) == 1) {
  return false;
}
  $bitmap = $bitmap + lshift(1, $code);
  $i = $i + 1;
};
  return true;
};
  echo rtrim(_str(is_contains_unique_chars('I_love.py'))), PHP_EOL;
  echo rtrim(_str(is_contains_unique_chars('I don\'t love Python'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
