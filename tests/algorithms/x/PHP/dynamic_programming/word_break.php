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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function build_set($words) {
  $m = [];
  foreach ($words as $w) {
  $m[$w] = true;
};
  return $m;
};
  function word_break($s, $words) {
  $n = strlen($s);
  $dict = build_set($words);
  $dp = [];
  $i = 0;
  while ($i <= $n) {
  $dp = _append($dp, false);
  $i = $i + 1;
};
  $dp[0] = true;
  $i = 1;
  while ($i <= $n) {
  $j = 0;
  while ($j < $i) {
  if ($dp[$j]) {
  $sub = substr($s, $j, $i - $j);
  if (array_key_exists($sub, $dict)) {
  $dp[$i] = true;
  $j = $i;
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $dp[$n];
};
  function print_bool($b) {
  if ($b) {
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim((false ? 'true' : 'false')), PHP_EOL;
}
};
  print_bool(word_break('applepenapple', ['apple', 'pen']));
  print_bool(word_break('catsandog', ['cats', 'dog', 'sand', 'and', 'cat']));
  print_bool(word_break('cars', ['car', 'ca', 'rs']));
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
