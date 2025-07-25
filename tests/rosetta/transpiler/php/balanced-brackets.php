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
$__start_mem = memory_get_usage();
$__start = _now();
  $seed = 1;
  function prng($max) {
  global $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed % $max;
};
  function gen($n) {
  global $seed;
  $arr = [];
  $i = 0;
  while ($i < $n) {
  $arr = array_merge($arr, ['[']);
  $arr = array_merge($arr, [']']);
  $i = $i + 1;
};
  $j = count($arr) - 1;
  while ($j > 0) {
  $k = prng($j + 1);
  $tmp = $arr[$j];
  $arr[$j] = $arr[$k];
  $arr[$k] = $tmp;
  $j = $j - 1;
};
  $out = '';
  foreach ($arr as $ch) {
  $out = $out . $ch;
};
  return $out;
};
  function testBalanced($s) {
  global $seed;
  $open = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c == '[') {
  $open = $open + 1;
} else {
  if ($c == ']') {
  if ($open == 0) {
  echo rtrim($s . ': not ok'), PHP_EOL;
  return;
};
  $open = $open - 1;
} else {
  echo rtrim($s . ': not ok'), PHP_EOL;
  return;
};
}
  $i = $i + 1;
};
  if ($open == 0) {
  echo rtrim($s . ': ok'), PHP_EOL;
} else {
  echo rtrim($s . ': not ok'), PHP_EOL;
}
};
  function main() {
  global $seed;
  $i = 0;
  while ($i < 10) {
  testBalanced(gen($i));
  $i = $i + 1;
};
  testBalanced('()');
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
