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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_split($s, $sep) {
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == $sep) {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  $res = _append($res, $current);
  return $res;
};
  function join_with_space($xs) {
  $s = '';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . $xs[$i];
  if ($i + 1 < count($xs)) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  return $s;
};
  function reverse_str($s) {
  $res = '';
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $res = $res . substr($s, $i, $i + 1 - $i);
  $i = $i - 1;
};
  return $res;
};
  function reverse_letters($sentence, $length) {
  $words = mochi_split($sentence, ' ');
  $result = [];
  $i = 0;
  while ($i < count($words)) {
  $word = $words[$i];
  if (strlen($word) > $length) {
  $result = _append($result, reverse_str($word));
} else {
  $result = _append($result, $word);
}
  $i = $i + 1;
};
  return join_with_space($result);
};
  function test_reverse_letters() {
  if (reverse_letters('Hey wollef sroirraw', 3) != 'Hey fellow warriors') {
  _panic('test1 failed');
}
  if (reverse_letters('nohtyP is nohtyP', 2) != 'Python is Python') {
  _panic('test2 failed');
}
  if (reverse_letters('1 12 123 1234 54321 654321', 0) != '1 21 321 4321 12345 123456') {
  _panic('test3 failed');
}
  if (reverse_letters('racecar', 0) != 'racecar') {
  _panic('test4 failed');
}
};
  function main() {
  test_reverse_letters();
  echo rtrim(reverse_letters('Hey wollef sroirraw', 3)), PHP_EOL;
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
