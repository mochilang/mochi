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
$__start_mem = memory_get_usage();
$__start = _now();
  function split_ws($s) {
  $res = [];
  $word = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ') {
  if ($word != '') {
  $res = _append($res, $word);
  $word = '';
};
} else {
  $word = $word . $ch;
}
  $i = $i + 1;
};
  if ($word != '') {
  $res = _append($res, $word);
}
  return $res;
};
  function mochi_contains($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function unique($xs) {
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  $w = $xs[$i];
  if (!mochi_contains($res, $w)) {
  $res = _append($res, $w);
}
  $i = $i + 1;
};
  return $res;
};
  function insertion_sort($arr) {
  $a = $arr;
  $i = 1;
  while ($i < count($a)) {
  $key = $a[$i];
  $j = $i - 1;
  while ($j >= 0 && $a[$j] > $key) {
  $a[$j + 1] = $a[$j];
  $j = $j - 1;
};
  $a[$j + 1] = $key;
  $i = $i + 1;
};
  return $a;
};
  function join_with_space($xs) {
  $s = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $s = $s . ' ';
}
  $s = $s . $xs[$i];
  $i = $i + 1;
};
  return $s;
};
  function remove_duplicates($sentence) {
  $words = split_ws($sentence);
  $uniq = unique($words);
  $sorted_words = insertion_sort($uniq);
  return join_with_space($sorted_words);
};
  echo rtrim(remove_duplicates('Python is great and Java is also great')), PHP_EOL;
  echo rtrim(remove_duplicates('Python   is      great and Java is also great')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
