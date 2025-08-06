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
  function get_value($keys, $values, $key) {
  $i = 0;
  while ($i < count($keys)) {
  if ($keys[$i] == $key) {
  return $values[$i];
}
  $i = $i + 1;
};
  return null;
};
  function contains_value($values, $value) {
  $i = 0;
  while ($i < count($values)) {
  if ($values[$i] == $value) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function backtrack($pattern, $input_string, $pi, $si, $keys, $values) {
  if ($pi == strlen($pattern) && $si == strlen($input_string)) {
  return true;
}
  if ($pi == strlen($pattern) || $si == strlen($input_string)) {
  return false;
}
  $ch = substr($pattern, $pi, $pi + 1 - $pi);
  $mapped = get_value($keys, $values, $ch);
  if ($mapped != null) {
  if (substr($input_string, $si, $si + strlen($mapped) - $si) == $mapped) {
  return backtrack($pattern, $input_string, $pi + 1, $si + strlen($mapped), $keys, $values);
};
  return false;
}
  $end = $si + 1;
  while ($end <= strlen($input_string)) {
  $substr = substr($input_string, $si, $end - $si);
  if (contains_value($values, $substr)) {
  $end = $end + 1;
  continue;
}
  $new_keys = _append($keys, $ch);
  $new_values = _append($values, $substr);
  if (backtrack($pattern, $input_string, $pi + 1, $end, $new_keys, $new_values)) {
  return true;
}
  $end = $end + 1;
};
  return false;
};
  function match_word_pattern($pattern, $input_string) {
  $keys = [];
  $values = [];
  return backtrack($pattern, $input_string, 0, 0, $keys, $values);
};
  function main() {
  echo rtrim(json_encode(match_word_pattern('aba', 'GraphTreesGraph'), 1344)), PHP_EOL;
  echo rtrim(json_encode(match_word_pattern('xyx', 'PythonRubyPython'), 1344)), PHP_EOL;
  echo rtrim(json_encode(match_word_pattern('GG', 'PythonJavaPython'), 1344)), PHP_EOL;
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
