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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function contains_int($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
};
  function contains_string($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
};
  function count_int($xs, $x) {
  $cnt = 0;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  $cnt = _iadd($cnt, 1);
}
  $i = _iadd($i, 1);
};
  return $cnt;
};
  function count_string($xs, $x) {
  $cnt = 0;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  $cnt = _iadd($cnt, 1);
}
  $i = _iadd($i, 1);
};
  return $cnt;
};
  function sort_int($xs) {
  $arr = $xs;
  $i = 0;
  while ($i < count($arr)) {
  $j = _iadd($i, 1);
  while ($j < count($arr)) {
  if ($arr[$j] < $arr[$i]) {
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
}
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  return $arr;
};
  function sort_string($xs) {
  $arr = $xs;
  $i = 0;
  while ($i < count($arr)) {
  $j = _iadd($i, 1);
  while ($j < count($arr)) {
  if ($arr[$j] < $arr[$i]) {
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
}
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
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
  $i = _iadd($i, 1);
};
  $max_count = 0;
  $i = 0;
  while ($i < count($counts)) {
  if ($counts[$i] > $max_count) {
  $max_count = $counts[$i];
}
  $i = _iadd($i, 1);
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
  $i = _iadd($i, 1);
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
  $i = _iadd($i, 1);
};
  $max_count = 0;
  $i = 0;
  while ($i < count($counts)) {
  if ($counts[$i] > $max_count) {
  $max_count = $counts[$i];
}
  $i = _iadd($i, 1);
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
  $i = _iadd($i, 1);
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
