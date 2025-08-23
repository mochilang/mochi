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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function reverse($s) {
  global $test_data;
  $res = '';
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $res = $res . substr($s, $i, $i + 1 - $i);
  $i = $i - 1;
};
  return $res;
};
  function is_palindrome($s) {
  global $test_data;
  $start_i = 0;
  $end_i = strlen($s) - 1;
  while ($start_i < $end_i) {
  if (substr($s, $start_i, $start_i + 1 - $start_i) == substr($s, $end_i, $end_i + 1 - $end_i)) {
  $start_i = $start_i + 1;
  $end_i = $end_i - 1;
} else {
  return false;
}
};
  return true;
};
  function is_palindrome_traversal($s) {
  global $test_data;
  $end = strlen($s) / 2;
  $n = strlen($s);
  $i = 0;
  while ($i < $end) {
  if (substr($s, $i, $i + 1 - $i) != substr($s, $n - $i - 1, $n - $i - 1 + 1 - ($n - $i - 1))) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function is_palindrome_recursive($s) {
  global $test_data;
  if (strlen($s) <= 1) {
  return true;
}
  if (substr($s, 0, 0 + 1) == substr($s, strlen($s) - 1, strlen($s) - 1 + 1 - (strlen($s) - 1))) {
  return is_palindrome_recursive(substr($s, 1, strlen($s) - 1 - 1));
}
  return false;
};
  function is_palindrome_slice($s) {
  global $test_data;
  return $s == reverse($s);
};
  $test_data = [['expected' => true, 'text' => 'MALAYALAM'], ['expected' => false, 'text' => 'String'], ['expected' => true, 'text' => 'rotor'], ['expected' => true, 'text' => 'level'], ['expected' => true, 'text' => 'A'], ['expected' => true, 'text' => 'BB'], ['expected' => false, 'text' => 'ABC'], ['expected' => true, 'text' => 'amanaplanacanalpanama']];
  function main() {
  global $test_data;
  foreach ($test_data as $t) {
  $s = $t['text'];
  $expected = $t['expected'];
  $r1 = is_palindrome($s);
  $r2 = is_palindrome_traversal($s);
  $r3 = is_palindrome_recursive($s);
  $r4 = is_palindrome_slice($s);
  if ($r1 != $expected || $r2 != $expected || $r3 != $expected || $r4 != $expected) {
  _panic('algorithm mismatch');
}
  echo rtrim($s . ' ' . _str($expected)), PHP_EOL;
};
  echo rtrim('a man a plan a canal panama'), PHP_EOL;
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
