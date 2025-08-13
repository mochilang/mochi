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
  function make_bool_list($n) {
  $row = [];
  $i = 0;
  while ($i < $n) {
  $row = _append($row, false);
  $i = $i + 1;
};
  return $row;
};
  function make_bool_matrix($rows, $cols) {
  $matrix = [];
  $i = 0;
  while ($i < $rows) {
  $matrix = _append($matrix, make_bool_list($cols));
  $i = $i + 1;
};
  return $matrix;
};
  function is_match($s, $p) {
  $n = strlen($s);
  $m = strlen($p);
  $dp = make_bool_matrix($n + 1, $m + 1);
  $dp[0][0] = true;
  $j = 1;
  while ($j <= $m) {
  if (substr($p, $j - 1, $j - ($j - 1)) == '*') {
  $dp[0][$j] = $dp[0][$j - 1];
}
  $j = $j + 1;
};
  $i = 1;
  while ($i <= $n) {
  $j2 = 1;
  while ($j2 <= $m) {
  $pc = substr($p, $j2 - 1, $j2 - ($j2 - 1));
  $sc = substr($s, $i - 1, $i - ($i - 1));
  if ($pc == $sc || $pc == '?') {
  $dp[$i][$j2] = $dp[$i - 1][$j2 - 1];
} else {
  if ($pc == '*') {
  if ($dp[$i - 1][$j2] || $dp[$i][$j2 - 1]) {
  $dp[$i][$j2] = true;
};
};
}
  $j2 = $j2 + 1;
};
  $i = $i + 1;
};
  return $dp[$n][$m];
};
  function print_bool($b) {
  if ($b) {
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim((false ? 'true' : 'false')), PHP_EOL;
}
};
  print_bool(is_match('abc', 'a*c'));
  print_bool(is_match('abc', 'a*d'));
  print_bool(is_match('baaabab', '*****ba*****ab'));
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
