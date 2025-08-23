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
    return intdiv(intval($a), intval($b));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pow2($n) {
  $res = 1;
  $i = 0;
  while ($i < $n) {
  $res = $res * 2;
  $i = $i + 1;
};
  return $res;
};
  function bit_and($a, $b) {
  $x = $a;
  $y = $b;
  $res = 0;
  $bit = 1;
  while ($x > 0 || $y > 0) {
  if ($x % 2 == 1 && $y % 2 == 1) {
  $res = $res + $bit;
}
  $x = intval((_intdiv($x, 2)));
  $y = intval((_intdiv($y, 2)));
  $bit = $bit * 2;
};
  return $res;
};
  function bit_or($a, $b) {
  $x = $a;
  $y = $b;
  $res = 0;
  $bit = 1;
  while ($x > 0 || $y > 0) {
  if ($x % 2 == 1 || $y % 2 == 1) {
  $res = $res + $bit;
}
  $x = intval((_intdiv($x, 2)));
  $y = intval((_intdiv($y, 2)));
  $bit = $bit * 2;
};
  return $res;
};
  function char_to_index($ch) {
  $letters = 'abcdefghijklmnopqrstuvwxyz';
  $i = 0;
  while ($i < strlen($letters)) {
  if (substr($letters, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return 26;
};
  function bitap_string_match($text, $pattern) {
  if ($pattern == '') {
  return 0;
}
  $m = strlen($pattern);
  if ($m > strlen($text)) {
  return -1;
}
  $limit = pow2($m + 1);
  $all_ones = $limit - 1;
  $pattern_mask = [];
  $i = 0;
  while ($i < 27) {
  $pattern_mask = _append($pattern_mask, $all_ones);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $m) {
  $ch = substr($pattern, $i, $i + 1 - $i);
  $idx = char_to_index($ch);
  $pattern_mask[$idx] = bit_and($pattern_mask[$idx], $all_ones - pow2($i));
  $i = $i + 1;
};
  $state = $all_ones - 1;
  $i = 0;
  while ($i < strlen($text)) {
  $ch = substr($text, $i, $i + 1 - $i);
  $idx = char_to_index($ch);
  $state = bit_or($state, $pattern_mask[$idx]);
  $state = ($state * 2) % $limit;
  if (bit_and($state, pow2($m)) == 0) {
  return $i - $m + 1;
}
  $i = $i + 1;
};
  return -1;
};
  function main() {
  echo rtrim(_str(bitap_string_match('abdabababc', 'ababc'))), PHP_EOL;
  echo rtrim(_str(bitap_string_match('abdabababc', ''))), PHP_EOL;
  echo rtrim(_str(bitap_string_match('abdabababc', 'c'))), PHP_EOL;
  echo rtrim(_str(bitap_string_match('abdabababc', 'fofosdfo'))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
