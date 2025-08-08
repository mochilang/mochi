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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function solution() {
  $targets = [1, 10, 100, 1000, 10000, 100000, 1000000];
  $idx = 0;
  $product = 1;
  $count = 0;
  $i = 1;
  while ($idx < count($targets)) {
  $s = _str($i);
  $j = 0;
  while ($j < strlen($s)) {
  $count = $count + 1;
  if ($count == $targets[$idx]) {
  $product = $product * ((ctype_digit(substr($s, $j, $j + 1 - $j)) ? intval(substr($s, $j, $j + 1 - $j)) : ord(substr($s, $j, $j + 1 - $j))));
  $idx = $idx + 1;
  if ($idx == count($targets)) {
  break;
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $product;
};
  function test_solution() {
  if (solution() != 210) {
  _panic('solution failed');
}
};
  function main() {
  test_solution();
  echo rtrim(_str(solution())), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
