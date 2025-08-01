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
$__start_mem = memory_get_usage();
$__start = _now();
  function bellTriangle($n) {
  $tri = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $i) {
  $row = array_merge($row, [0]);
  $j = $j + 1;
};
  $tri = array_merge($tri, [$row]);
  $i = $i + 1;
};
  $tri[1][0] = 1;
  $i = 2;
  while ($i < $n) {
  $tri[$i][0] = $tri[$i - 1][$i - 2];
  $j = 1;
  while ($j < $i) {
  $tri[$i][$j] = $tri[$i][$j - 1] + $tri[$i - 1][$j - 1];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $tri;
};
  function main() {
  $bt = bellTriangle(51);
  echo rtrim('First fifteen and fiftieth Bell numbers:'), PHP_EOL;
  for ($i = 1; $i < 16; $i++) {
  echo rtrim('' . str_pad(_str($i), 2, ' ', STR_PAD_LEFT) . ': ' . _str($bt[$i][0])), PHP_EOL;
};
  echo rtrim('50: ' . _str($bt[50][0])), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  echo rtrim('The first ten rows of Bell\'s triangle:'), PHP_EOL;
  for ($i = 1; $i < 11; $i++) {
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($bt[$i], 1344))))))), PHP_EOL;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
