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
  function contains_int($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function contains_string($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function count_int($xs, $x) {
  $cnt = 0;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  $cnt = $cnt + 1;
}
  $i = $i + 1;
};
  return $cnt;
};
  function count_string($xs, $x) {
  $cnt = 0;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  $cnt = $cnt + 1;
}
  $i = $i + 1;
};
  return $cnt;
};
  function sort_int($xs) {
  $arr = $xs;
  $i = 0;
  while ($i < count($arr)) {
  $j = $i + 1;
  while ($j < count($arr)) {
  if ($arr[$j] < $arr[$i]) {
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function sort_string($xs) {
  $arr = $xs;
  $i = 0;
  while ($i < count($arr)) {
  $j = $i + 1;
  while ($j < count($arr)) {
  if ($arr[$j] < $arr[$i]) {
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function mode_int($lst) {
  if (count($lst) == 0) {
  return [];
}
  $counts = [];
  $i = 0;
  while ($i < count($lst)) {
  $counts = _append($counts, count_int($lst, $lst[$i]));
  $i = $i + 1;
};
  $max_count = 0;
  $i = 0;
  while ($i < count($counts)) {
  if ($counts[$i] > $max_count) {
  $max_count = $counts[$i];
}
  $i = $i + 1;
};
  $modes = [];
  $i = 0;
  while ($i < count($lst)) {
  if ($counts[$i] == $max_count) {
  $v = $lst[$i];
  if (!contains_int($modes, $v)) {
  $modes = _append($modes, $v);
};
}
  $i = $i + 1;
};
  return sort_int($modes);
};
  function mode_string($lst) {
  if (count($lst) == 0) {
  return [];
}
  $counts = [];
  $i = 0;
  while ($i < count($lst)) {
  $counts = _append($counts, count_string($lst, $lst[$i]));
  $i = $i + 1;
};
  $max_count = 0;
  $i = 0;
  while ($i < count($counts)) {
  if ($counts[$i] > $max_count) {
  $max_count = $counts[$i];
}
  $i = $i + 1;
};
  $modes = [];
  $i = 0;
  while ($i < count($lst)) {
  if ($counts[$i] == $max_count) {
  $v = $lst[$i];
  if (!contains_string($modes, $v)) {
  $modes = _append($modes, $v);
};
}
  $i = $i + 1;
};
  return sort_string($modes);
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(mode_int([2, 3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 2, 2, 2]), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(mode_int([3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 2, 2, 2]), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(mode_int([3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 4, 2, 2, 4, 2]), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(mode_string(['x', 'y', 'y', 'z']), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(mode_string(['x', 'x', 'y', 'y', 'z']), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
