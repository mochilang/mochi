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
  function to_binary($n) {
  global $data;
  if ($n == 0) {
  return '0';
}
  $num = $n;
  $res = '';
  while ($num > 0) {
  $bit = $num % 2;
  $res = _str($bit) . $res;
  $num = _intdiv($num, 2);
};
  return $res;
};
  function contains_key_int($m, $key) {
  global $data;
  foreach (array_keys($m) as $k) {
  if ($k == $key) {
  return true;
}
};
  return false;
};
  function lzw_compress($bits) {
  global $data;
  $dict = ['0' => 0, '1' => 1];
  $current = '';
  $result = '';
  $index = 2;
  $i = 0;
  while ($i < strlen($bits)) {
  $ch = substr($bits, $i, $i + 1 - $i);
  $candidate = $current . $ch;
  if (contains_key_int($dict, $candidate)) {
  $current = $candidate;
} else {
  $result = $result . to_binary($dict[$current]);
  $dict[$candidate] = $index;
  $index = $index + 1;
  $current = $ch;
}
  $i = $i + 1;
};
  if ($current != '') {
  $result = $result . to_binary($dict[$current]);
}
  return $result;
};
  $data = '01001100100111';
  echo rtrim(lzw_compress($data)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
