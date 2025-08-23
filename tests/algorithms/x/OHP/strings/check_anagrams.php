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
$__start_mem = memory_get_usage();
$__start = _now();
  function strip_and_remove_spaces($s) {
  $start = 0;
  $end = strlen($s) - 1;
  while ($start < strlen($s) && substr($s, $start, $start + 1 - $start) == ' ') {
  $start = $start + 1;
};
  while ($end >= $start && substr($s, $end, $end + 1 - $end) == ' ') {
  $end = $end - 1;
};
  $res = '';
  $i = $start;
  while ($i <= $end) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch != ' ') {
  $res = $res . $ch;
}
  $i = $i + 1;
};
  return $res;
};
  function check_anagrams($a, $b) {
  $s1 = strtolower($a);
  $s2 = strtolower($b);
  $s1 = strip_and_remove_spaces($s1);
  $s2 = strip_and_remove_spaces($s2);
  if (strlen($s1) != strlen($s2)) {
  return false;
}
  $count = [];
  $i = 0;
  while ($i < strlen($s1)) {
  $c1 = substr($s1, $i, $i + 1 - $i);
  $c2 = substr($s2, $i, $i + 1 - $i);
  if (array_key_exists($c1, $count)) {
  $count[$c1] = $count[$c1] + 1;
} else {
  $count[$c1] = 1;
}
  if (array_key_exists($c2, $count)) {
  $count[$c2] = $count[$c2] - 1;
} else {
  $count[$c2] = -1;
}
  $i = $i + 1;
};
  foreach (array_keys($count) as $ch) {
  if ($count[$ch] != 0) {
  return false;
}
};
  return true;
};
  function print_bool($b) {
  if ($b) {
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim((false ? 'true' : 'false')), PHP_EOL;
}
};
  print_bool(check_anagrams('Silent', 'Listen'));
  print_bool(check_anagrams('This is a string', 'Is this a string'));
  print_bool(check_anagrams('This is    a      string', 'Is     this a string'));
  print_bool(check_anagrams('There', 'Their'));
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
