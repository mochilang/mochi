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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function range_list($n) {
  $lst = [];
  $i = 0;
  while ($i < $n) {
  $lst = _append($lst, $i);
  $i = $i + 1;
};
  return $lst;
};
  function min3($a, $b, $c) {
  $m = $a;
  if ($b < $m) {
  $m = $b;
}
  if ($c < $m) {
  $m = $c;
}
  return $m;
};
  function levenshtein_distance($first_word, $second_word) {
  if (strlen($first_word) < strlen($second_word)) {
  return levenshtein_distance($second_word, $first_word);
}
  if (strlen($second_word) == 0) {
  return strlen($first_word);
}
  $previous_row = range_list(strlen($second_word) + 1);
  $i = 0;
  while ($i < strlen($first_word)) {
  $c1 = substr($first_word, $i, $i + 1 - $i);
  $current_row = [];
  $current_row = _append($current_row, $i + 1);
  $j = 0;
  while ($j < strlen($second_word)) {
  $c2 = substr($second_word, $j, $j + 1 - $j);
  $insertions = $previous_row[$j + 1] + 1;
  $deletions = $current_row[$j] + 1;
  $substitutions = $previous_row[$j] + (($c1 == $c2 ? 0 : 1));
  $min_val = min3($insertions, $deletions, $substitutions);
  $current_row = _append($current_row, $min_val);
  $j = $j + 1;
};
  $previous_row = $current_row;
  $i = $i + 1;
};
  return $previous_row[count($previous_row) - 1];
};
  function levenshtein_distance_optimized($first_word, $second_word) {
  if (strlen($first_word) < strlen($second_word)) {
  return levenshtein_distance_optimized($second_word, $first_word);
}
  if (strlen($second_word) == 0) {
  return strlen($first_word);
}
  $previous_row = range_list(strlen($second_word) + 1);
  $i = 0;
  while ($i < strlen($first_word)) {
  $c1 = substr($first_word, $i, $i + 1 - $i);
  $current_row = [];
  $current_row = _append($current_row, $i + 1);
  $k = 0;
  while ($k < strlen($second_word)) {
  $current_row = _append($current_row, 0);
  $k = $k + 1;
};
  $j = 0;
  while ($j < strlen($second_word)) {
  $c2 = substr($second_word, $j, $j + 1 - $j);
  $insertions = $previous_row[$j + 1] + 1;
  $deletions = $current_row[$j] + 1;
  $substitutions = $previous_row[$j] + (($c1 == $c2 ? 0 : 1));
  $min_val = min3($insertions, $deletions, $substitutions);
  $current_row[$j + 1] = $min_val;
  $j = $j + 1;
};
  $previous_row = $current_row;
  $i = $i + 1;
};
  return $previous_row[count($previous_row) - 1];
};
  function main() {
  $a = 'kitten';
  $b = 'sitting';
  echo rtrim(_str(levenshtein_distance($a, $b))), PHP_EOL;
  echo rtrim(_str(levenshtein_distance_optimized($a, $b))), PHP_EOL;
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
