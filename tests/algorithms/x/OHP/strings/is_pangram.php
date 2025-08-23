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
$__start_mem = memory_get_usage();
$__start = _now();
  function is_pangram($input_str) {
  global $s1, $s2;
  $letters = [];
  $i = 0;
  while ($i < strlen($input_str)) {
  $c = strtolower(substr($input_str, $i, $i + 1 - $i));
  $is_new = !(in_array($c, $letters));
  if ($c != ' ' && 'a' <= $c && $c <= 'z' && $is_new) {
  $letters = _append($letters, $c);
}
  $i = $i + 1;
};
  return count($letters) == 26;
};
  function is_pangram_faster($input_str) {
  global $s1, $s2;
  $alphabet = 'abcdefghijklmnopqrstuvwxyz';
  $flag = [];
  $i = 0;
  while ($i < 26) {
  $flag = _append($flag, false);
  $i = $i + 1;
};
  $j = 0;
  while ($j < strlen($input_str)) {
  $c = strtolower(substr($input_str, $j, $j + 1 - $j));
  $k = 0;
  while ($k < 26) {
  if (substr($alphabet, $k, $k + 1 - $k) == $c) {
  $flag[$k] = true;
  break;
}
  $k = $k + 1;
};
  $j = $j + 1;
};
  $t = 0;
  while ($t < 26) {
  if (!$flag[$t]) {
  return false;
}
  $t = $t + 1;
};
  return true;
};
  function is_pangram_fastest($input_str) {
  global $s1, $s2;
  $s = strtolower($input_str);
  $alphabet = 'abcdefghijklmnopqrstuvwxyz';
  $i = 0;
  while ($i < strlen($alphabet)) {
  $letter = substr($alphabet, $i, $i + 1 - $i);
  if (!(strpos($s, $letter) !== false)) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  $s1 = 'The quick brown fox jumps over the lazy dog';
  $s2 = 'My name is Unknown';
  echo rtrim(_str(is_pangram($s1))), PHP_EOL;
  echo rtrim(_str(is_pangram($s2))), PHP_EOL;
  echo rtrim(_str(is_pangram_faster($s1))), PHP_EOL;
  echo rtrim(_str(is_pangram_faster($s2))), PHP_EOL;
  echo rtrim(_str(is_pangram_fastest($s1))), PHP_EOL;
  echo rtrim(_str(is_pangram_fastest($s2))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
