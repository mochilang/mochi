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
$_dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/strings";
function _read_file($path) {
    $p = $path;
    if (!file_exists($p) && isset($_dataDir)) {
        $p = $_dataDir . DIRECTORY_SEPARATOR . $path;
    }
    $data = @file_get_contents($p);
    if ($data === false) return '';
    return $data;
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_split($s, $sep) {
  global $word_by_signature;
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
  function insertion_sort($arr) {
  global $word_by_signature;
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
  function sort_chars($word) {
  global $word_by_signature;
  $chars = [];
  $i = 0;
  while ($i < strlen($word)) {
  $chars = _append($chars, $word[$i]);
  $i = $i + 1;
};
  $chars = insertion_sort($chars);
  $res = '';
  $i = 0;
  while ($i < count($chars)) {
  $res = $res . $chars[$i];
  $i = $i + 1;
};
  return $res;
};
  function unique_sorted($words) {
  global $word_by_signature;
  $seen = [];
  $res = [];
  foreach ($words as $w) {
  if ($w != '' && $not(array_key_exists($w, $seen))) {
  $res = _append($res, $w);
  $seen[$w] = true;
}
};
  $res = insertion_sort($res);
  return $res;
};
  $word_by_signature = [];
  function build_map($words) {
  global $word_by_signature;
  foreach ($words as $w) {
  $sig = sort_chars($w);
  $arr = [];
  if (array_key_exists($sig, $word_by_signature)) {
  $arr = $word_by_signature[$sig];
}
  $arr = _append($arr, $w);
  $word_by_signature[$sig] = $arr;
};
};
  function anagram($my_word) {
  global $word_by_signature;
  $sig = sort_chars($my_word);
  if (array_key_exists($sig, $word_by_signature)) {
  return $word_by_signature[$sig];
}
  return [];
};
  function main() {
  global $word_by_signature;
  $text = _read_file('words.txt');
  $lines = mochi_split($text, '
');
  $words = unique_sorted($lines);
  build_map($words);
  foreach ($words as $w) {
  $anas = anagram($w);
  if (count($anas) > 1) {
  $line = $w . ':';
  $i = 0;
  while ($i < count($anas)) {
  if ($i > 0) {
  $line = $line . ',';
}
  $line = $line . $anas[$i];
  $i = $i + 1;
};
  echo rtrim($line), PHP_EOL;
}
};
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
